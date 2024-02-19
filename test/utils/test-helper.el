;;; test-helper.el --- Clojure TS Mode: Non-interactive unit-test setup  -*- lexical-binding: t; -*-

;; Copyright © 2022-2024 Bozhidar Batsov <bozhidar@batsov.dev>

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
  "Create a temporary buffer, insert TEXT,switch to clojure-ts-mode.
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
  (declare (indent 2))
  `(progn
     (with-clojure-ts-buffer ,text
                             (goto-char (point-min))
                             (re-search-forward "|")
                             (delete-char -1)
                             ,@body)))

(provide 'test-helper)
;;; test-helper.el ends here
