;;; indent-check.el --- Check the indentation of the sources  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reindents every Emacs Lisp source file of the project and reports the lines
;; that differ from what is checked in.  Run it with `eldev indent', or `eldev
;; indent --fix' to have the offenders rewritten in place.
;;
;; Buttercup and the test suite have to be loaded before anything is checked.
;; How a macro call gets indented comes from the `declare' form of the macro,
;; which only exists once that macro is defined, so without them `describe',
;; `it' and the helpers in test-helper.el all fall back to the default function
;; indentation and practically every spec looks misindented.
;;
;; Indentation depends on the Emacs version, so the verdict that counts is the
;; one from the Emacs the lint job runs.

;;; Code:

(require 'buttercup)
(require 'clojure-ts-mode)
(require 'subr-x)

(defconst clojure-ts-mode-indent-check-file (or load-file-name buffer-file-name)
  "The file this code lives in, so that it can skip loading itself.")

(defconst clojure-ts-mode-indent-check-excluded-dirs
  '(".git" ".eldev" "clojure-mode-tests")
  "Directories that hold no Emacs Lisp of ours.")

(defconst clojure-ts-mode-indent-check-lone-comment-regexp
  (rx bos (* blank) ";" (or (not (any ";")) eos))
  "Matches a line holding a comment that opens with a single semicolon.
`indent-region' aligns those to `comment-column' instead of to the
surrounding code, which is not something worth policing.")

(defun clojure-ts-mode-indent-check--test-dir ()
  "Return the directory holding the test suite."
  (file-name-directory clojure-ts-mode-indent-check-file))

(defun clojure-ts-mode-indent-check--project-dir ()
  "Return the root directory of the project."
  (file-name-directory (directory-file-name
                        (clojure-ts-mode-indent-check--test-dir))))

(defun clojure-ts-mode-indent-check--load-macros ()
  "Load the test suite, so that its macros carry indentation metadata."
  (let ((test-dir (clojure-ts-mode-indent-check--test-dir)))
    (add-to-list 'load-path test-dir)
    (dolist (file (directory-files test-dir t "\\.el\\'"))
      (unless (equal file clojure-ts-mode-indent-check-file)
        (load file nil t)))))

(defun clojure-ts-mode-indent-check--files ()
  "Return the Emacs Lisp source files of the project, in a stable order."
  (let ((files (directory-files-recursively
                (clojure-ts-mode-indent-check--project-dir)
                "\\.el\\'"
                nil
                (lambda (dir)
                  (not (member (file-name-nondirectory (directory-file-name dir))
                               clojure-ts-mode-indent-check-excluded-dirs))))))
    (sort (seq-remove (lambda (file)
                        (string-suffix-p "-autoloads.el" file))
                      files)
          #'string<)))

(defun clojure-ts-mode-indent-check--exempt-p (line)
  "Return non-nil if the indentation of LINE should be left alone.
Blank lines would only gain trailing whitespace, and a lone semicolon
comment gets aligned to `comment-column' rather than to the code."
  (or (string-blank-p line)
      (string-match-p clojure-ts-mode-indent-check-lone-comment-regexp line)))

(defun clojure-ts-mode-indent-check--indent (lines file)
  "Return LINES of FILE as Emacs would indent them.
FILE is only used to give the temporary buffer a name, so that
file-local variables and the like resolve the way they do on disk."
  (let ((indented (with-temp-buffer
                    (setq buffer-file-name file)
                    (insert (string-join lines "\n"))
                    (emacs-lisp-mode)
                    (setq-local indent-tabs-mode nil)
                    ;; The progress reporter would drown out the report.
                    (let ((inhibit-message t))
                      (indent-region (point-min) (point-max)))
                    (setq buffer-file-name nil)
                    (split-string (buffer-string) "\n"))))
    ;; `indent-region' only ever rewrites leading whitespace, so the two lists
    ;; line up.  Bail out rather than guess if that ever stops holding.
    (unless (= (length lines) (length indented))
      (error "Reindenting %s changed the number of lines" file))
    (seq-mapn (lambda (before after)
                (if (clojure-ts-mode-indent-check--exempt-p before) before after))
              lines indented)))

(defun clojure-ts-mode-indent-check--diverging-lines (before after)
  "Return the numbers of the lines where the lists BEFORE and AFTER differ."
  (let ((line 0)
        (lines nil))
    (while (or before after)
      (setq line (1+ line))
      (unless (equal (car before) (car after))
        (push line lines))
      (setq before (cdr before)
            after (cdr after)))
    (nreverse lines)))

(defun clojure-ts-mode-indent-check (&optional fix)
  "Check that the Emacs Lisp sources of the project are correctly indented.

Report the misindented lines of every file, rewriting the file when FIX
is non-nil.  Return the number of files that needed reindenting."
  (clojure-ts-mode-indent-check--load-macros)
  (let ((offenders 0))
    (dolist (file (clojure-ts-mode-indent-check--files))
      (let* ((lines (split-string (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string))
                                  "\n"))
             (indented (clojure-ts-mode-indent-check--indent lines file))
             (diverging (clojure-ts-mode-indent-check--diverging-lines
                         lines indented)))
        (when diverging
          (setq offenders (1+ offenders))
          (message "%s: %s"
                   (file-relative-name
                    file (clojure-ts-mode-indent-check--project-dir))
                   (mapconcat #'number-to-string diverging ", "))
          (when fix
            (write-region (string-join indented "\n") nil file nil 'silent)))))
    (cond
     ((zerop offenders) (message "Indentation is fine."))
     (fix (message "Reindented %d file(s)." offenders)))
    offenders))

(provide 'indent-check)
;;; indent-check.el ends here
