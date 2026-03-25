;;; clojure-ts-mode-config-test.el --- Clojure TS Mode: configuration test suite  -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

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

;; Tests for clojure-ts-mode configuration options (defcustom variables).

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

;;;; clojure-ts-toplevel-inside-comment-form

(describe "clojure-ts-toplevel-inside-comment-form"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "should treat the comment form as a defun when nil (default)"
    (with-clojure-ts-buffer "(comment\n  (defn foo [] 1)\n  (defn bar [] 2))"
      ;; Point inside the comment form
      (goto-char 15)
      (let ((node (treesit-defun-at-point)))
        ;; The defun-at-point should be the comment form
        (expect node :not :to-be nil)
        (expect (string-prefix-p "(comment" (treesit-node-text node))
                :to-be-truthy))))

  (it "should navigate to inner defns when non-nil"
    (with-clojure-ts-buffer "(comment\n  (defn foo [] 1)\n  (defn bar [] 2))"
      (setq-local clojure-ts-toplevel-inside-comment-form t)
      ;; Point inside the comment form
      (goto-char 15)
      (let ((node (treesit-defun-at-point)))
        ;; The defun-at-point should be the inner defn, not the comment
        (expect node :not :to-be nil)
        (expect (string-prefix-p "(defn foo" (treesit-node-text node))
                :to-be-truthy)))))

;;;; clojure-ts-docstring-fill-column

(describe "clojure-ts-docstring-fill-column"
  (it "should use the custom fill column when filling docstrings"
    (with-clojure-ts-buffer "(defn foo
  \"This is a short docstring.\"
  [])"
      (let ((clojure-ts-docstring-fill-column 20))
        (goto-char (point-min))
        (search-forward "This")
        (fill-paragraph)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "(defn foo
  \"This is a short
  docstring.\"
  [])"))))

  (it "should not affect comment filling"
    (with-clojure-ts-buffer ";; This is a comment that should not be affected by the docstring fill column setting."
      (let ((clojure-ts-docstring-fill-column 10)
            (fill-column 70))
        (goto-char (point-min))
        (fill-paragraph)
        ;; Comment wraps at fill-column (70), not docstring-fill-column (10)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal ";; This is a comment that should not be affected by the docstring fill
;; column setting.")))))

;;;; clojure-ts-docstring-fill-prefix-width

(describe "clojure-ts-docstring-fill-prefix-width"
  (it "should use custom fill prefix width when filling docstrings"
    (with-clojure-ts-buffer "(defn foo
  \"This is a short docstring.\"
  [])"
      (let ((clojure-ts-docstring-fill-column 20)
            (clojure-ts-docstring-fill-prefix-width 4))
        (goto-char (point-min))
        (search-forward "This")
        (fill-paragraph)
        ;; Continuation lines should be indented with 4 spaces
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "(defn foo
  \"This is a short
    docstring.\"
  [])"))))

  (it "should default to 2 spaces for fill prefix"
    (with-clojure-ts-buffer "(defn foo
  \"This is a short docstring.\"
  [])"
      (let ((clojure-ts-docstring-fill-column 20))
        (goto-char (point-min))
        (search-forward "This")
        (fill-paragraph)
        ;; Default: continuation lines indented with 2 spaces
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "(defn foo
  \"This is a short
  docstring.\"
  [])")))))

;;;; clojure-ts-thread-all-but-last

(describe "clojure-ts-thread-all-but-last"
  (it "should thread all expressions by default"
    (with-clojure-ts-buffer "(map inc (filter even? coll))"
      (goto-char (point-min))
      (clojure-ts-thread-last-all nil)
      (expect (buffer-string)
              :to-equal "(->> coll\n     (filter even?)\n     (map inc))")))

  (it "should leave last expression unthreaded when set to t"
    (with-clojure-ts-buffer "(map inc (filter even? coll))"
      (let ((clojure-ts-thread-all-but-last t))
        (goto-char (point-min))
        (clojure-ts-thread-last-all nil))
      (expect (buffer-string)
              :to-equal "(->> (filter even? coll)\n     (map inc))")))

  (it "should apply to thread-first-all as well"
    (with-clojure-ts-buffer "(assoc (merge {} {:a 1}) :b 2)"
      (let ((clojure-ts-thread-all-but-last t))
        (goto-char (point-min))
        (clojure-ts-thread-first-all nil))
      (expect (buffer-string)
              :to-equal "(-> (merge {} {:a 1})\n    (assoc :b 2))"))))

;;; clojure-ts-mode-config-test.el ends here
