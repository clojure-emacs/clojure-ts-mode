(ns indentation
  "Docstring `important`. asdf"
  (:require
   [clojure.string :as str])
  (:import
   (java.util Date
              UUID)))

; Single ; comment
;; double ; comment
(when something
  ; single ; comment
  ;; double ; comment
  body)

(defmethod dispatch :on-me
  [x]
  (bar x))

(defn f [x]
  body)

(defn f
  ([x] (f x nil))
  ([x y] ))

(defn f
  [x]
  body)

(defn many-args [a b c
                 d e f]
  body)

(defn multi-arity
  ([x]
   body)
  ([x y]
   body))

(let [x 1
      y 2]
  body)

[1 2 3
 4 5 6]

{:key-1 v1
 :key-2 v2}

#{a b c
  d e f}


(or (condition-a)
    (condition-b))

(filter even? (range 1 10))

(clojure.core/filter even?
                     (range 1 10))

(#'filter even?
          (range 10))

(filter
 even?
 (range 1 10))

(asdf
 asdf
 asdf)

(defn foo [x 1]
  x)

(:my/keyword
 {:my/keyword 1
  :another-keyword 2}
 "default value")

(defprotocol IProto
  (foo [this x]
    "`this` is a docstring.")
  (bar [this y]))

(deftype MyThing []
  IProto
  (foo [this x]
    x))

(defrecord MyThingR []
  IProto
  (foo [this x] x))

(defn foo2 [x]b)

(reify
  IProto
  (foo [this x]
    x))

(extend-type MyThing
  clojure.lang.IFn
  (invoke [this] 1))

(extend-protocol clojure.lang.IFn
  MyThingR
  (invoke [this]
    2))

(extend AType
  AProtocol
  {:foo an-existing-fn
   :bar (fn [a b]
          a)
   :baz (fn
          ([a] a)
          ([a b]
           b))})

^:foo
(def a 1)

^{:foo true}
(def b 2)

^{:foo true}
[1 2]

(comment
  ^{:a 1}
  (def a 2))

(defn hinted
  (^String [])
  (^java.util.List
   [a & args]))

^{:foo true}
(defn c
  "hello"
  [_foo]
  (+ 1 1))

;;; Block 0 rule

(do (aligned)
    (vertically))

(do
  (indented)
  (with-2-spaces))

(future
  (body is indented))

(try
  (something)
  ;; A bit of block 2 rule
  (catch Exception e
    "Third argument is indented with 2 spaces.")
  (catch ExceptionInfo
         e-info
    "Second argument is aligned vertically with the first one."))

;;; Block 1 rule

(case x
  2 (print 2)
  3 (print 3)
  (print "Default"))

(cond-> {}
  :always (assoc :hello "World")
  false (do nothing))

(with-precision 32
  (/ (bigdec 20) (bigdec 30)))

(testing "Something should work"
  (is (something-working?)))

;;; Block 2 rule

(are [x y]
     (= x y)
  2 3
  4 5
  6 6)

(as-> {} $
  (assoc $ :hello "World"))

(as-> {}
      my-map
  (assoc my-map :hello "World"))

;;; Inner 0 rule

(fn named-lambda [x]
  (+ x x))

(defmethod hello :world
  [arg1 arg2]
  (+ arg1 arg2))

(reify
  AutoCloseable
  (close
    [this]
    (is properly indented)))
