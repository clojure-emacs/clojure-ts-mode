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

  (when-refactoring-it "should work with a namespace-qualified def form"
    "(clojure.core/defn add [a b]
  (+ a b))"

    "(clojure.core/defn- add [a b]
  (+ a b))"

    (clojure-ts-cycle-privacy))

  (when-refactoring-with-point-it "should leave the next definition alone when point isn't in one"
    "(ns |foo)

(defn bar [] 1)"
    "(ns |foo)

(defn bar [] 1)"

    (expect (clojure-ts-cycle-privacy) :to-throw 'user-error))

  (when-refactoring-with-point-it "should not mistake a name starting with def for a def form"
    "(ns my.|default)"
    "(ns my.|default)"

    (expect (clojure-ts-cycle-privacy) :to-throw 'user-error))

  (when-refactoring-with-point-it "should leave a spec definition alone"
    "(s/def ::f|oo int?)"
    "(s/def ::f|oo int?)"

    (expect (clojure-ts-cycle-privacy) :to-throw 'user-error))

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

(describe "clojure-cycle-if"

  (when-refactoring-with-point-it "should cycle inner if"
    "(if this
  (if |that
    (then AAA)
    (else BBB))
  (otherwise CCC))"

    "(if this
  (if-not |that
    (else BBB)
    (then AAA))
  (otherwise CCC))"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should cycle outer if"
    "(if-not |this
  (if that
    (then AAA)
    (else BBB))
  (otherwise CCC))"

    "(if |this
  (otherwise CCC)
  (if that
    (then AAA)
    (else BBB)))"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should cycle a namespace-qualified if"
    "(clojure.core/if-not |a b c)"
    "(clojure.core/if |a c b)"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should swap the branches around a discarded form"
    "(if |a #_x b c)"
    "(if-not |a #_x c b)"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should move metadata along with its branch"
    "(if |a ^String b c)"
    "(if-not |a c ^String b)"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should move metadata along with its branch past a comment"
    "(if |a ^String ;; comment
  b c)"
    "(if-not |a c ^String ;; comment
  b)"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should refuse to swap the branches around a reader conditional"
    "(if |a b #?(:clj c))"
    "(if |a b #?(:clj c))"

    (expect (clojure-ts-cycle-conditional) :to-throw 'user-error))

  (when-refactoring-with-point-it "should make a missing else branch explicit"
    "(if |a b)"
    "(if-not |a nil b)"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should cycle an if in a function literal"
    "#(if |% 1 2)"
    "#(if-not |% 2 1)"

    (clojure-ts-cycle-conditional)))

(describe "clojure-cycle-when"

  (when-refactoring-with-point-it "should cycle inner when"
    "(when this
  (when |that
    (aaa)
    (bbb))
  (ccc))"

    "(when this
  (when-not |that
    (aaa)
    (bbb))
  (ccc))"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should cycle outer when"
    "(when-not |this
  (when that
    (aaa)
    (bbb))
  (ccc))"

    "(when |this
  (when that
    (aaa)
    (bbb))
  (ccc))"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should cycle a namespace-qualified when"
    "(clojure.core/when |a b)"
    "(clojure.core/when-not |a b)"

    (clojure-ts-cycle-conditional))

  (when-refactoring-with-point-it "should cycle a when around a reader conditional"
    "(when |a #?(:clj b))"
    "(when-not |a #?(:clj b))"

    (clojure-ts-cycle-conditional)))

(describe "clojure-cycle-not"

  (when-refactoring-with-point-it "should add a not when missing"
    "(ala bala| portokala)"
    "(not (ala bala| portokala))"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should remove a not when present"
    "(not (ala bala| portokala))"
    "(ala bala| portokala)"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should put the metadata of the form inside a new not"
    "(when ^:x (foo |a) b)"
    "(when (not ^:x (foo |a)) b)"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should keep the metadata of the form when removing a not"
    "(when (not ^:x (foo |a)) b)"
    "(when ^:x (foo |a) b)"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should keep metadata separated from the form by a comment"
    "(when (not ^:x ;; comment
            (foo |a)) b)"
    "(when ^:x ;; comment
  (foo |a) b)"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should add a not around a form whose head has metadata"
    "(^String fo|o a)"
    "(not (^String fo|o a))"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should remove a not around a form whose head has metadata"
    "(not (^String fo|o a))"
    "(^String fo|o a)"

    (clojure-ts-cycle-not))

  (when-refactoring-with-point-it "should remove a namespace-qualified not"
    "(when (clojure.core/not (foo |a)) b)"
    "(when (foo |a) b)"

    (clojure-ts-cycle-not)))

(describe "cycling with point between forms"

  (when-refactoring-with-point-it "should not make the next definition private"
    "(defn f [x] x)\n|\n(defn g [x] x)"
    "(defn f [x] x)\n|\n(defn g [x] x)"

    (expect (clojure-ts-cycle-privacy) :to-throw 'user-error))

  (when-refactoring-with-point-it "should not turn the last string into a keyword"
    "\"foo\"\n\n|"
    "\"foo\"\n\n|"

    (expect (clojure-ts-cycle-keyword-string) :to-throw 'user-error))

  (when-refactoring-with-point-it "should not make the last definition private"
    "(defn f [x] x)\n\n|"
    "(defn f [x] x)\n\n|"

    (expect (clojure-ts-cycle-privacy) :to-throw 'user-error))

  (when-refactoring-with-point-it "should not add a not to the last form"
    "(foo a)\n\n|"
    "(foo a)\n\n|"

    (expect (clojure-ts-cycle-not) :to-throw 'user-error)))

(provide 'clojure-ts-mode-cycling-test)
;;; clojure-ts-mode-cycling-test.el ends here
