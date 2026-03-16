;;; clojure-ts-mode-node-child-skip-metadata-test.el --- Tests for node-child-skip-metadata  -*- lexical-binding: t; -*-

;; Copyright © 2026 Dieter Komendera

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

;; Tests for `clojure-ts--node-child-skip-metadata'.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(defun clojure-ts-test--first-list-node ()
  "Return the first list_lit node in the current buffer."
  (car (mapcar #'cdr (treesit-query-capture
                      (treesit-buffer-root-node) "(list_lit) @n"))))

(describe "clojure-ts--node-child-skip-metadata"
  (it "returns the first value child of a simple list"
    (with-clojure-ts-buffer "(defn foo [x] x)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "defn")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "foo"))))

  (it "skips metadata on the symbol (^:private)"
    (with-clojure-ts-buffer "(defn ^:private foo [x] x)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "defn")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "foo"))))

  (it "skips multiple metadata annotations"
    (with-clojure-ts-buffer "(defn ^:private ^:dynamic foo [x] x)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "defn")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "foo"))))

  (it "skips type-hint metadata"
    (with-clojure-ts-buffer "(defn ^String foo [x] x)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "defn")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "foo"))))

  (it "skips map metadata on the symbol"
    (with-clojure-ts-buffer "(defn ^{:doc \"hello\"} foo [x] x)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "defn")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "foo"))))

  (it "works with def forms"
    (with-clojure-ts-buffer "(def ^:private my-var 42)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "def")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "my-var"))))

  (it "works with ns forms"
    (with-clojure-ts-buffer "(ns ^{:doc \"my ns\"} my.namespace)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "ns")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "my.namespace"))))

  (it "returns nil when n is out of range"
    (with-clojure-ts-buffer "(foo)"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (clojure-ts--node-child-skip-metadata list-node 0)
                :not :to-be nil)
        (expect (clojure-ts--node-child-skip-metadata list-node 1)
                :to-be nil))))

  (it "works with non-symbol value children"
    (with-clojure-ts-buffer "(def foo [1 2 3])"
      (let ((list-node (clojure-ts-test--first-list-node)))
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 0))
                :to-equal "def")
        (expect (treesit-node-text (clojure-ts--node-child-skip-metadata list-node 1))
                :to-equal "foo")))))

;;; clojure-ts-mode-node-child-skip-metadata-test.el ends here
