;;; clojure-ts-mode-refactor-add-arity-test.el --- Clojure[TS] Mode: refactor add arity test.  -*- lexical-binding: t; -*-

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

;; Test for `clojure-ts-add-arity'

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(describe "clojure-ts-add-arity"

  (when-refactoring-with-point-it "should add an arity to a single-arity defn with args on same line"
    "(defn foo [arg]
  body|)"

    "(defn foo
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should add an arity to a single-arity defn with args on next line"
    "(defn foo
  [arg]
  bo|dy)"

    "(defn foo
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defn with a docstring"
    "(defn foo
  \"some docst|ring\"
  [arg]
  body)"

    "(defn foo
  \"some docstring\"
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defn with metadata"
    "(defn fo|o
  ^{:bla \"meta\"}
  [arg]
  body)"

    "(defn foo
  ^{:bla \"meta\"}
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defn with multiple body expressions"
    "(defn fo|o
  ^{:bla \"meta\"}
  [arg]
  body
  second-expr)"

    "(defn foo
  ^{:bla \"meta\"}
  ([|])
  ([arg]
   body
   second-expr))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should add an arity to a multi-arity defn"
    "(defn foo
  ([arg1])
  ([ar|g1 arg2]
   body))"

    "(defn foo
  ([|])
  ([arg1])
  ([arg1 arg2]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity defn with a docstring"
    "(defn foo
  \"some docstring\"
  ([])
  ([arg|]
   body))"

    "(defn foo
  \"some docstring\"
  ([|])
  ([])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity defn with metadata"
    "(defn foo
  \"some docstring\"
  ^{:bla \"meta\"}
  ([])
  |([arg]
   body))"

    "(defn foo
  \"some docstring\"
  ^{:bla \"meta\"}
  ([|])
  ([])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity fn"
    "(fn foo [arg]
  body|)"

    "(fn foo
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity fn"
    "(fn foo
  ([x y]
   body)
  ([a|rg]
   body))"

    "(fn foo
  ([|])
  ([x y]
   body)
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defmacro"
    "(defmacro foo [arg]
  body|)"

    "(defmacro foo
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity defmacro"
    "(defmacro foo
  ([x y]
   body)
  ([a|rg]
   body))"

    "(defmacro foo
  ([|])
  ([x y]
   body)
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defmethod"
    "(defmethod foo :bar [arg]
  body|)"

    "(defmethod foo :bar
  ([|])
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity defmethod"
    "(defmethod foo :bar
  ([x y]
   body)
  ([a|rg]
   body))"

    "(defmethod foo :bar
  ([|])
  ([x y]
   body)
  ([arg]
   body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a defn inside a reader conditional"
    "#?(:clj
   (defn foo
     \"some docstring\"
     ^{:bla \"meta\"}
     |([arg]
      body)))"

    "#?(:clj
   (defn foo
     \"some docstring\"
     ^{:bla \"meta\"}
     ([|])
     ([arg]
      body)))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a defn inside a reader conditional with 2 platform tags"
    "#?(:clj
   (defn foo
     \"some docstring\"
     ^{:bla \"meta\"}
     |([arg]
      body))
   :cljs
   (defn foo
     \"some docstring\"
     ^{:bla \"meta\"}
     ([arg]
      body)))"

    "#?(:clj
   (defn foo
     \"some docstring\"
     ^{:bla \"meta\"}
     ([|])
     ([arg]
      body))
   :cljs
   (defn foo
     \"some docstring\"
     ^{:bla \"meta\"}
     ([arg]
      body)))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity fn inside a letfn"
    "(letfn [(foo [x]
           bo|dy)]
  (foo 3))"

    "(letfn [(foo
          ([|])
          ([x]
           body))]
  (foo 3))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity fn inside a letfn"
    "(letfn [(foo
          ([x]
           body)
          |([x y]
           body))]
  (foo 3))"

    "(letfn [(foo
          ([|])
          ([x]
           body)
          ([x y]
           body))]
  (foo 3))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a proxy"
    "(proxy [Foo] []
  (bar [arg]
     body|))"

    "(proxy [Foo] []
  (bar
    ([|])
    ([arg]
     body)))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a defprotocol"
    "(defprotocol Foo
  \"some docstring\"
  (bar [arg] [x |y] \"some docstring\"))"

    "(defprotocol Foo
  \"some docstring\"
  (bar [|] [arg] [x y] \"some docstring\"))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle a reify"
    "(reify Foo
  (bar [arg] body)
  (blahs [arg]| body))"

    "(reify Foo
  (bar [arg] body)
  (blahs [|])
  (blahs [arg] body))"

    (clojure-ts-add-arity))

  (when-refactoring-with-point-it "should handle an extend-protocol"
    "(extend-protocol prepare/SettableParameter
  clojure.lang.IPersistentMap
  (set-parameter [m ^PreparedStatement s i]
    (.setObject| s i (->pgobject m))))"

    "(extend-protocol prepare/SettableParameter
  clojure.lang.IPersistentMap
  (set-parameter [|])
  (set-parameter [m ^PreparedStatement s i]
    (.setObject s i (->pgobject m))))"

    (clojure-ts-add-arity))

  (it "should signal a user error when point is not inside a function body"
    (with-clojure-ts-buffer-point "
(letf|n [(foo
          ([x]
           body)
          ([x y]
           body))]
  (foo 3))"
      (expect (clojure-ts-add-arity)
              :to-throw
              'user-error
              '("No suitable form to add an arity at point")))

    (with-clojure-ts-buffer-point "
(defprotocol Fo|o
  \"some docstring\"
  (bar [arg] [x y] \"some docstring\"))"
      (expect (clojure-ts-add-arity)
              :to-throw
              'user-error
              '("No suitable form to add an arity at point")))))

(provide 'clojure-ts-mode-refactor-add-arity-test)
;;; clojure-ts-mode-refactor-add-arity-test.el ends here
