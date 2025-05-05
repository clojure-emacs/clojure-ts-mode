;;; clojure-ts-mode-convert-collection-test.el --- Clojure[TS] Mode convert collection type.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roman Rudakov

;; Author: Roman Rudakov <rrudakov@fastmail.com>

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

;; Adapted from `clojure-mode'.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(describe "clojure-ts-convert-collection-to-map"
  (when-refactoring-it "should convert a list to a map"
    "(:a 1 :b 2)"
    "{:a 1 :b 2}"
    (backward-sexp)
    (down-list)
    (clojure-ts-convert-collection-to-map))

  (it "should signal a user error when there is no collection at point"
    (with-clojure-ts-buffer "false"
      (backward-sexp)
      (expect (clojure-ts-convert-collection-to-map)
              :to-throw
              'user-error
              '("No collection at point to convert")))))

(describe "clojure-ts-convert-collection-to-vector"
  (when-refactoring-it "should convert a map to a vector"
    "{:a 1 :b 2}"
    "[:a 1 :b 2]"
    (backward-sexp)
    (down-list)
    (clojure-ts-convert-collection-to-vector))

  (it "should signal a user error when there is no collection at point"
    (with-clojure-ts-buffer "false"
      (backward-sexp)
      (expect (clojure-ts-convert-collection-to-vector)
              :to-throw
              'user-error
              '("No collection at point to convert")))))

(describe "clojure-ts-convert-collection-to-set"
  (when-refactoring-it "should convert a vector to a set"
    "[1 2 3]"
    "#{1 2 3}"
    (backward-sexp)
    (down-list)
    (clojure-ts-convert-collection-to-set))

  (when-refactoring-it "should convert a quoted list to a set"
    "'(1 2 3)"
    "#{1 2 3}"
    (backward-sexp)
    (down-list)
    (clojure-ts-convert-collection-to-set))

  (it "should signal a user error when there is no collection at point"
    (with-clojure-ts-buffer "false"
      (backward-sexp)
      (expect (clojure-ts-convert-collection-to-set)
              :to-throw
              'user-error
              '("No collection at point to convert")))))

(describe "clojure-ts-convert-collection-to-list"
  (when-refactoring-it "should convert a set to a list"
    "#{1 2 3}"
    "(1 2 3)"
    (backward-sexp)
    (down-list)
    (clojure-ts-convert-collection-to-list))

  (it "should signal a user error when there is no collection at point"
    (with-clojure-ts-buffer "false"
      (backward-sexp)
      (expect (clojure-ts-convert-collection-to-list)
              :to-throw
              'user-error
              '("No collection at point to convert")))))

(describe "clojure-ts-convert-collection-to-quoted-list"
  (when-refactoring-it "should convert a set to a quoted list"
    "#{1 2 3}"
    "'(1 2 3)"
    (backward-sexp)
    (down-list)
    (clojure-ts-convert-collection-to-quoted-list))

  (it "should signal a user error when there is no collection at point"
    (with-clojure-ts-buffer "false"
      (backward-sexp)
      (expect (clojure-ts-convert-collection-to-quoted-list)
              :to-throw
              'user-error
              '("No collection at point to convert")))))


(provide 'clojure-ts-mode-convert-collection-test)
;;; clojure-ts-mode-convert-collection-test.el ends here
