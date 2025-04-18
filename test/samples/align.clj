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
