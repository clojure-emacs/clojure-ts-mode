;;; test-helper.el --- Clojure TS Mode: Non-interactive unit-test setup  -*- lexical-binding: t; -*-

;; Copyright © 2022-2025 Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Non-interactive test suite setup.

;;; Code:

(defmacro with-clojure-ts-buffer (text &rest body)
  "Create a temporary buffer, insert TEXT, switch to `clojure-ts-mode'.

And evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (erase-buffer)
     (insert ,text)
     (clojure-ts-mode)
     ,@body))

(defmacro with-clojure-ts-buffer-point (text &rest body)
  "Run BODY in a temporary clojure buffer with TEXT.

TEXT is a string with a | indicating where point is.  The | will be erased
and point left there."
  (declare (indent 1))
  `(progn
     (with-clojure-ts-buffer ,text
       (goto-char (point-min))
       (re-search-forward "|")
       (delete-char -1)
       ,@body)))

(defun clojure-ts--s-index-of (needle s &optional ignore-case)
  "Returns first index of NEEDLE in S, or nil.

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search ignore-case))
    (string-match-p (regexp-quote needle) s)))

(defmacro when-refactoring-it (description before after &rest body)
  "Return a buttercup spec.

Insert BEFORE into a buffer, evaluate BODY and compare the resulting buffer to
AFTER.

BODY should contain the refactoring that transforms BEFORE into AFTER.

DESCRIPTION is the description of the spec."
  (declare (indent 1))
  `(it ,description
     (with-clojure-ts-buffer ,before
       ,@body
       (expect (buffer-string) :to-equal ,after))))

(defmacro when-refactoring-with-point-it (description before after &rest body)
  "Return a buttercup spec.

Like when-refactor-it but also checks whether point is moved to the expected
position.

BEFORE is the buffer string before refactoring, where a pipe (|) represents
point.

AFTER is the expected buffer string after refactoring, where a pipe (|)
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
         ,@body
         (expect (buffer-string) :to-equal expected-state)
         (expect (point) :to-equal expected-cursor-pos)))))


;; https://emacs.stackexchange.com/a/55031
(defmacro with-temp-dir (temp-dir &rest body)
  "Create a temporary directory and bind its to TEMP-DIR while evaluating BODY.
Removes the temp directory at the end of evaluation."
  `(let ((,temp-dir (make-temp-file "" t)))
    (unwind-protect
      (progn
        ,@body)
      (delete-directory ,temp-dir t))))

(provide 'test-helper)
;;; test-helper.el ends here
