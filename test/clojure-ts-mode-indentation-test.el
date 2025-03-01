;;; clojure-ts-mode-indentation-test.el --- Clojure TS Mode: indentation test suite  -*- lexical-binding: t; -*-

;; Copyright © 2022-2025 Danny Freeman

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

;; The unit test suite of Clojure TS Mode

(require 'clojure-ts-mode)
(require 'cl-lib)
(require 'buttercup)
(require 's nil t)               ;Don't burp if it's missing during compilation.


(defmacro when-indenting-with-point-it (description before after)
  "Return a buttercup spec.

Check whether the swift indentation command will correctly change the buffer.
Will also check whether point is moved to the expected position.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
    (let* ((after ,after)
           (expected-cursor-pos (1+ (clojure-ts--s-index-of "|" after)))
           (expected-state (delete ?| after)))
      (with-clojure-ts-buffer ,before
        (goto-char (point-min))
        (search-forward "|")
        (delete-char -1)
        (font-lock-ensure)
        (indent-according-to-mode)
        (expect (buffer-string) :to-equal expected-state)
        (expect (point) :to-equal expected-cursor-pos)))))



;; Backtracking indent
(defmacro when-indenting-it (description &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-ts-mode)
                      (insert "\n" ,form);,(replace-regexp-in-string "\n +" "\n " form))
                      (indent-region (point-min) (point-max))
                      (expect (buffer-string) :to-equal ,(concat "\n" form))))
                 forms))))


;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "when-indenting-with-point-it") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "when-indenting-with-point-it") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))


(describe "indentation"
  (it "should not hang on end of buffer"
    (with-clojure-ts-buffer "(let [a b]"
      (goto-char (point-max))
      (expect
       (with-timeout (2)
         (newline-and-indent)
         t))))

  (when-indenting-with-point-it "should have no indentation at top level"
    "|x"

    "|x")

  (when-indenting-it "should handle non-symbol at start"
    "
{\"1\" 2
 *3 4}")

  (when-indenting-it "should have no indentation at top level lists with metadata"
    "
^{:foo true}
(def b 2)")

  (when-indenting-it "should have no indentation at top level vectors with metadata"
    "
^{:foo true}
[1 2]")

  (when-indenting-it "should have no indentation at top level maps with metadata"
    "
^{:foo true}
{:a 1}")

  (when-indenting-it "should have no indentation with metadata inside comment"
    "
(comment
  ^{:a 1}
  (def a 2))")

  (when-indenting-it "should have params, docstring and body correctly indented in presence of metadata"
    "
^{:foo true}
(defn c
  \"hello\"
  [_foo]
  (+ 1 1))")

(when-indenting-it "should support function calls via vars"
   "
(#'foo 5
       6)"))
