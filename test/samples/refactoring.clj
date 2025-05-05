(ns refactoring)

;;; Threading

;;;; Unwind

(-> ;; This is comment
    (foo)
    ;; Another comment
    (bar true
         ;; Hello
         false)
    (baz))


(let [some (->> yeah
                (world foo
                       false)
                hello)])

(->> coll
     (filter identity)
     (map :id)
     (map :name))

(some->> coll
         (filter identity)
         (map :id)
         (map :name))

(defn plus [a b]
  (-> a (+ b)))

(some->> :b
         (find {:a 1}) val
         (+ 5))

(some->> (val (find {:a 1} :b))
         (+ 5))

;;;; Thread

(-> (foo (bar (baz)) "arg on a separate line"))

(foo (bar (baz)))

(-> (foo (bar))
    (baz))

(->> (filter :active? (map :status items)))

(-> (dissoc (assoc {} :key "value") :lock))


(-> inc
    (map [1 2]))

(map
 inc
 [1 2])

#(-> (.-value (.-target %)))

(->> (range)
     (map inc))

(->> (map square (filter even? [1 2 3 4 5])))

(-> (dissoc (assoc {} :key "value") :lock))

(deftask dev []
         (comp (serve)
               (cljs (lala)
                     10)))

(def my-name "Roma")

(defn say-hello
  []
  (println "Hello" my-name))

(definline bad-sqr [x] `(* ~x ~x))

(defmulti service-charge (juxt account-level :tag))

;; Convert collections.

#{1 2 3}

[1 2 3]

;; TODO: Define indentation rule for `ns_map_lit`
#:hello{:name "Roma"
 :world true}
