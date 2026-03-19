;;; clojure-ts-mode-navigation-test.el --- Clojure TS Mode: navigation test suite  -*- lexical-binding: t; -*-

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

;; Navigation test suite for clojure-ts-mode: defun movement,
;; forward-sexp, defun-name, and which-func integration.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

;;;; beginning-of-defun / end-of-defun

(describe "navigation: beginning-of-defun"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "moves to the start of the current defn"
    (with-clojure-ts-buffer "(defn foo [x]
  (+ x 1))

(defn bar [y]
  (* y 2))"
      (goto-char (point-max))
      (beginning-of-defun)
      (expect (looking-at "(defn bar") :to-be-truthy)))

  (it "moves past multiple defuns"
    (with-clojure-ts-buffer "(defn a [] 1)

(defn b [] 2)

(defn c [] 3)"
      (goto-char (point-max))
      (beginning-of-defun 2)
      (expect (looking-at "(defn b") :to-be-truthy)))

  (it "moves across different definition types"
    (with-clojure-ts-buffer "(ns test)

(def x 1)

(defn foo [] 42)

(defmacro bar [& body] body)"
      (goto-char (point-max))
      (beginning-of-defun)
      (expect (looking-at "(defmacro bar") :to-be-truthy)
      (beginning-of-defun)
      (expect (looking-at "(defn foo") :to-be-truthy)
      (beginning-of-defun)
      (expect (looking-at "(def x") :to-be-truthy))))

(describe "navigation: end-of-defun"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "moves to the end of the current defn"
    (with-clojure-ts-buffer "(defn foo [x]
  (+ x 1))

(defn bar [y]
  (* y 2))"
      (goto-char (point-min))
      (end-of-defun)
      ;; Should be past the first defn
      (expect (>= (point)
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "(+ x 1))")
                    (point)))
              :to-be-truthy)))

  (it "moves to the end of a multi-line definition"
    (with-clojure-ts-buffer "(defn multi
  [x]
  (let [a 1
        b 2]
    (+ a b x)))

(def next-thing 1)"
      (goto-char (point-min))
      (end-of-defun)
      (expect (>= (point)
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "(+ a b x)))")
                    (point)))
              :to-be-truthy))))

;;;; forward-sexp

(describe "navigation: forward-sexp"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "moves over a list expression"
    (with-clojure-ts-buffer "(+ 1 2)"
      (goto-char (point-min))
      (forward-sexp)
      (expect (eobp) :to-be-truthy)))

  (it "moves over a vector"
    (with-clojure-ts-buffer "[1 2 3]"
      (goto-char (point-min))
      (forward-sexp)
      (expect (eobp) :to-be-truthy)))

  (it "moves over a map"
    (with-clojure-ts-buffer "{:a 1 :b 2}"
      (goto-char (point-min))
      (forward-sexp)
      (expect (eobp) :to-be-truthy)))

  (it "moves over consecutive top-level forms"
    (with-clojure-ts-buffer "(defn a [] 1)

(defn b [] 2)"
      (goto-char (point-min))
      (forward-sexp)
      (expect (looking-at "\n\n(defn b") :to-be-truthy)))

  (it "moves backward over a list expression"
    (with-clojure-ts-buffer "(+ 1 2)"
      (goto-char (point-max))
      (backward-sexp)
      (expect (bobp) :to-be-truthy)))

  (it "moves over elements inside a list"
    (with-clojure-ts-buffer "(foo \"hello\" [1 2])"
      (goto-char (point-min))
      (down-list)
      ;; Now inside the list, after (
      (forward-sexp)  ;; over foo
      (forward-sexp)  ;; over "hello"
      (expect (looking-at " \\[1 2\\]") :to-be-truthy))))

;;;; defun-name

(describe "navigation: defun-name"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "returns the name of a defn"
    (with-clojure-ts-buffer "(defn my-function [x] x)"
      (let ((node (treesit-node-at 2)))
        ;; Navigate up to the list node
        (while (and node (not (string= (treesit-node-type node) "list_lit")))
          (setq node (treesit-node-parent node)))
        (expect (clojure-ts--standard-definition-node-name node)
                :to-equal "my-function"))))

  (it "returns the name of a def"
    (with-clojure-ts-buffer "(def my-var 42)"
      (let ((node (treesit-node-at 2)))
        (while (and node (not (string= (treesit-node-type node) "list_lit")))
          (setq node (treesit-node-parent node)))
        (expect (clojure-ts--standard-definition-node-name node)
                :to-equal "my-var"))))

  (it "returns the name of a ns form"
    (with-clojure-ts-buffer "(ns my.ns)"
      (let ((node (treesit-node-at 2)))
        (while (and node (not (string= (treesit-node-type node) "list_lit")))
          (setq node (treesit-node-parent node)))
        (expect (clojure-ts--standard-definition-node-name node)
                :to-equal "my.ns"))))

  (it "returns nil for non-definition nodes"
    (with-clojure-ts-buffer "[1 2 3]"
      (let ((node (treesit-node-at 1)))
        (while (and node (not (string= (treesit-node-type node) "vec_lit")))
          (setq node (treesit-node-parent node)))
        (expect (clojure-ts--standard-definition-node-name node)
                :to-be nil)))))

;;;; which-func / add-log integration

(describe "navigation: which-func"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "returns the current defun name via add-log-current-defun"
    (with-clojure-ts-buffer "(defn foo [x]
  (+ x 1))

(defn bar [y]
  (* y 2))"
      (goto-char (point-min))
      (search-forward "+ x")
      (expect (add-log-current-defun) :to-equal "foo")))

  (it "returns the current def name via add-log-current-defun"
    (with-clojure-ts-buffer "(def my-config
  {:host \"localhost\"
   :port 8080})"
      (goto-char (point-min))
      (search-forward "localhost")
      (expect (add-log-current-defun) :to-equal "my-config"))))

;;;; outline integration

(describe "navigation: outline"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar not available")))

  (it "sets treesit-outline-predicate when outline variant is comments"
    (let ((clojure-ts-outline-variant 'comments))
      (with-clojure-ts-buffer ";;; heading\n(def x 1)"
        (expect treesit-outline-predicate :not :to-be nil))))

  (it "outline-next-heading moves to the next heading"
    (let ((clojure-ts-outline-variant 'comments))
      (with-clojure-ts-buffer ";;; First heading

(defn foo [] 1)

;;; Second heading

(defn bar [] 2)

;;; Third heading"
        (goto-char (point-min))
        (outline-minor-mode 1)
        (outline-next-heading)
        (expect (looking-at ";;; Second") :to-be-truthy)
        (outline-next-heading)
        (expect (looking-at ";;; Third") :to-be-truthy)))))

;;; clojure-ts-mode-navigation-test.el ends here
