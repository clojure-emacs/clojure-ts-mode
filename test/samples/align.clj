(ns align)

(let [^long my-map {:hello  "World"   ;Hello
                    :foo
                    ^String (str "Foo" "Bar")
                    :number ^long 132
                    :zz     "hello"}
      another      {:this ^{:hello "world"} "is"
                    :a    #long "1234"
                    :b    {:this   "is"
                           :nested "map"}}])


{:foo "bar", :baz "Hello"
 :a   "b"    :c   "d"}


(clojure.core/with-redefs [hello "world"
                           foo   "bar"]
  (println hello foo))

(condp = 2
  123   "Hello"
  99999 "World"
  234   nil)

(let [a-long-name 10
      b           20])

#?(:clj  2
   :cljs 2)

#?@(:clj  [2]
    :cljs [4])

(let [this-is-a-form b
      c              d

      another form
      k       g])

{:this-is-a-form b
 c               d

 :another form
 k        g}

(let [x  {:a 1
          :b 2} ; comment
      xx 3]
  x)

(case x
  :a  (let [a  1
            aa (+ a 1)]
        aa); comment
  :aa 2)

{:map  "with"
 :some #_"ignored" "form"}

{:map      "with"
 :multiple "ignored"
 #_#_:forms "foo"}
