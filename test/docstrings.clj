(ns clojure-ts-mode.docstrings
  "This is a namespace
  See my famous `fix-bug` macro if you need help."
  (:require [clojure.test :refer [deftest]])
  (:import (java.util UUID)))

(def foo ;;asdf
  "I'm a value")
(def bar "I'm a docstring" "and I'm a value")

(defonce ^{:doc "gotta document in metadata."} baz
  "Did you know defonce doesn't have a docstring arity like def?")

(def foobar
  ;; Comments shouldn't disrupt docstring highlighting
  "I'm a docstring"
  123)

(defn ;;asdf
  foobarbaz ;;asdf
  "I'm the docstring!" ;;asdf
  [x]
  (inc x))

(;; starting comments break docstrings
 defn busted!
 "We really need to anchor symbols like defn to the front of the list.
I don't want every query to have to check for comments.
Don't format code this way."
 []
 nil)

(defn buzz "Looking for `fizz`"
  [x]
  (when (zero? (% x 5))
    "buzz"))

(defn- fizz
  "Pairs well with `buzz`"
  [x]
  (when (zero? (% x 3))
    "fizz"))

(defmacro fix-bug
  "Fixes most known bugs."
  [& body]
  `(try
     ~@body
     (catch Throwable _
       nil)))

(definline never-used-this ":)" [x] x)

(deftype ^{:doc "asdf" :something-else "asdf"} T
  java.lang.Closeable
  (close [this]
    (print "done")))

(defprotocol Fooable
  (foo [this]
    "Does foo"))

(definterface Barable
  (^String bar [] "Does bar"))

(deftest ^{:doc "doctest"} some-test
  (is (= 1 2)))
