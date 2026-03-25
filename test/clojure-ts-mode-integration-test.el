;;; clojure-ts-mode-integration-test.el --- Clojure TS Mode: integration test suite  -*- lexical-binding: t; -*-

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

;; Integration tests that load sample resource files and verify that
;; font-lock and imenu work correctly end-to-end.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'imenu)
(require 'test-helper "test/test-helper")

(describe "integration: indentation"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "preserves correct indentation of indentation.clj after indent-region"
    (let* ((file (clojure-ts-test--resource-file "indentation.clj"))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (clojure-ts-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "preserves correct indentation of outline.clj after indent-region"
    (let* ((file (clojure-ts-test--resource-file "outline.clj"))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (clojure-ts-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original)))))

(describe "integration: font-lock on sample files"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "applies expected font-lock faces to indentation.clj"
    (let ((file (clojure-ts-test--resource-file "indentation.clj")))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (clojure-ts-mode))
        (font-lock-ensure)
        ;; Check that "defn" keyword is fontified
        (goto-char (point-min))
        (search-forward "(defn f")
        (expect (get-text-property (1+ (match-beginning 0)) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check that a docstring is fontified
        (goto-char (point-min))
        (search-forward "\"Docstring")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-doc-face)
        ;; Check that a comment is fontified
        (goto-char (point-min))
        (search-forward ";; double")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-comment-face))))

  (it "applies expected font-lock faces to docstrings.clj"
    (let ((file (clojure-ts-test--resource-file "docstrings.clj")))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 3))
          (clojure-ts-mode))
        (font-lock-ensure)
        ;; ns docstring
        (goto-char (point-min))
        (search-forward "This is a namespace")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-doc-face)
        ;; def with docstring before value
        (goto-char (point-min))
        (search-forward "I'm a docstring")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-doc-face)
        ;; defmacro docstring
        (goto-char (point-min))
        (search-forward "Fixes most known bugs")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-doc-face)
        ;; defprotocol method docstring
        (goto-char (point-min))
        (search-forward "Does foo")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-doc-face))))

  (it "applies expected font-lock faces to test.clj"
    (let ((file (clojure-ts-test--resource-file "test.clj")))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 3))
          (clojure-ts-mode))
        (font-lock-ensure)
        ;; Check that "ns" is fontified
        (goto-char (point-min))
        (search-forward "(ns ")
        (expect (get-text-property (1+ (match-beginning 0)) 'face)
                :to-equal 'font-lock-keyword-face)))))

(describe "integration: imenu on sample files"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "builds imenu index from indentation.clj"
    (let ((file (clojure-ts-test--resource-file "indentation.clj")))
      (with-temp-buffer
        (insert-file-contents file)
        (clojure-ts-mode)
        (let ((index (imenu--make-index-alist)))
          ;; Should have Function and Variable categories
          (expect (assoc "Function" index) :not :to-be nil)
          (expect (assoc "Variable" index) :not :to-be nil)))))

  (it "builds imenu index from refactoring.clj"
    (let ((file (clojure-ts-test--resource-file "refactoring.clj")))
      (with-temp-buffer
        (insert-file-contents file)
        (clojure-ts-mode)
        (let* ((index (imenu--make-index-alist))
               (flatten-index (imenu--flatten-index-alist index t)))
          ;; Should find the namespace
          (expect (assoc "Namespace:refactoring" flatten-index) :not :to-be nil))))))

(describe "integration: outline on sample files"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "navigates headings in outline.clj"
    (let ((file (clojure-ts-test--resource-file "outline.clj")))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((clojure-ts-outline-variant 'comments))
          (clojure-ts-mode))
        (outline-minor-mode 1)
        (goto-char (point-min))
        (outline-next-heading)
        (expect (looking-at ";;; First heading") :to-be-truthy)
        (outline-next-heading)
        (expect (looking-at ";;;; Heading level 2") :to-be-truthy)
        (outline-next-heading)
        (expect (looking-at ";;; Second heading") :to-be-truthy)))))

;;; clojure-ts-mode-integration-test.el ends here
