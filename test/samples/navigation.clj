(ns navigation)

(let [my-var ^{:foo "bar"} (= "Hello" "Hello")])

(let [my-var ^boolean (= "Hello" "world")])

#(+ % %)

^boolean (= 2 2)

(defn- to-string
  ^String
  [arg]
  (.toString arg))
