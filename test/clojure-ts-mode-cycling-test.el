;;; clojure-ts-mode-cycling-test.el --- Clojure[TS] Mode: cycling things tests  -*- lexical-binding: t; -*-

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

;; The code is adapted from `clojure-mode'.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(describe "clojure-ts-cycle-keyword-string"
  (when-refactoring-with-point-it "should convert string to keyword"
    "\"hel|lo\""

    ":hel|lo"

    (clojure-ts-cycle-keyword-string))

  (when-refactoring-with-point-it "should convert keyword to string"
    ":|hello"

    "\"|hello\""

    (clojure-ts-cycle-keyword-string))

  (it "should signal a user error when there is nothing to convert at point"
    (with-clojure-ts-buffer "[true false]"
      (goto-char 2)
      (expect (clojure-ts-cycle-keyword-string)
              :to-throw
              'user-error
              '("No string or keyword at point"))))

  (it "should signal a user error when string at point contains spaces"
    (with-clojure-ts-buffer "\"Hello world\""
      (goto-char 2)
      (expect (clojure-ts-cycle-keyword-string)
              :to-throw
              'user-error
              '("Cannot convert a string containing spaces to keyword")))))

(describe "clojure-ts-cycle-privacy"

  (when-refactoring-it "should turn a public defn into a private defn"
    "(defn add [a b]
  (+ a b))"

    "(defn- add [a b]
  (+ a b))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should also work from the beginning of a sexp"
     "(defn- add [a b]
  (+ a b))"

     "(defn add [a b]
  (+ a b))"

     (backward-sexp)
     (clojure-ts-cycle-privacy))

  (when-refactoring-it "should use metadata when clojure-use-metadata-for-privacy is set to true"
    "(defn add [a b]
  (+ a b))"

    "(defn ^:private add [a b]
  (+ a b))"

    (let ((clojure-ts-use-metadata-for-defn-privacy t))
      (clojure-ts-cycle-privacy)))

  (when-refactoring-it "should turn a private defn into a public defn"
    "(defn- add [a b]
  (+ a b))"

    "(defn add [a b]
  (+ a b))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a private defn with metadata into a public defn"
    "(defn ^:private add [a b]
  (+ a b))"

    "(defn add [a b]
  (+ a b))"

    (let ((clojure-ts-use-metadata-for-defn-privacy t))
      (clojure-ts-cycle-privacy)))

  (when-refactoring-it "should also work with pre-existing metadata"
    "(def ^:dynamic config
  \"docs\"
  {:env \"staging\"})"

    "(def ^:private ^:dynamic config
  \"docs\"
  {:env \"staging\"})"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a private def with metadata into a public def"
    "(def ^:private config
  \"docs\"
  {:env \"staging\"})"

    "(def config
  \"docs\"
  {:env \"staging\"})"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a public defmulti into a private defmulti"
    "(defmulti service-charge (juxt account-level :tag))"

    "(defmulti ^:private service-charge (juxt account-level :tag))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a private defmulti into a public defmulti"
    "(defmulti ^:private service-charge (juxt account-level :tag))"

    "(defmulti service-charge (juxt account-level :tag))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a public defmacro into a private defmacro"
    "(defmacro unless [pred a b]
  `(if (not ~pred) ~a ~b))"

    "(defmacro ^:private unless [pred a b]
  `(if (not ~pred) ~a ~b))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a private defmacro into a public defmacro"
    "(defmacro ^:private unless [pred a b]
  `(if (not ~pred) ~a ~b))"

    "(defmacro unless [pred a b]
  `(if (not ~pred) ~a ~b))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a private definline into a public definline"
    "(definline bad-sqr [x] `(* ~x ~x))"

    "(definline ^:private bad-sqr [x] `(* ~x ~x))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a public definline into a private definline"
    "(definline ^:private bad-sqr [x] `(* ~x ~x))"

    "(definline bad-sqr [x] `(* ~x ~x))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a private defrecord into a public defrecord"
    "(defrecord Person [fname lname address])"

    "(defrecord ^:private Person [fname lname address])"

    (clojure-ts-cycle-privacy))

  (when-refactoring-it "should turn a public defrecord into a private defrecord"
    "(defrecord ^:private Person [fname lname address])"

    "(defrecord Person [fname lname address])"

    (clojure-ts-cycle-privacy)))

(provide 'clojure-ts-mode-cycling-test)
;;; clojure-ts-mode-cycling-test.el ends here
