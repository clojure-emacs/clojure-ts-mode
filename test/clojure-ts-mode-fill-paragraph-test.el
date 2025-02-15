;;; clojure-ts-mode-fill-paragraph-test.el ---       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roman Rudakov

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

;; The unit test suite of CLojure TS Mode

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)

(describe "clojure-ts--fill-paragraph"
  (it "should reformat only the docstring"
    (with-clojure-ts-buffer "(ns foo)

(defn hello-world
  \"This is a very long docstring that should be reformatted using fill-paragraph function.\"
  []
  (pringln \"Hello world\"))"
                            (goto-char 40)
                            (prog-fill-reindent-defun)
                            (expect (buffer-substring-no-properties (point-min) (point-max))
                                    :to-equal
                                    "(ns foo)

(defn hello-world
  \"This is a very long docstring that should be reformatted using
  fill-paragraph function.\"
  []
  (pringln \"Hello world\"))")))

  (it "should reformat normal comments properly"
    (with-clojure-ts-buffer "(ns foo)

;; This is a very long comment that should be reformatted using fill-paragraph function."
                              (goto-char 20)
                              (prog-fill-reindent-defun)
                              (expect (buffer-substring-no-properties (point-min) (point-max))
                                      :to-equal
                                      "(ns foo)

;; This is a very long comment that should be reformatted using
;; fill-paragraph function."))))

;;; clojure-ts-mode-fill-paragraph-test.el ends here
