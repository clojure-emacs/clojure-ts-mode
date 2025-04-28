(ns refactoring)

;;; Threading

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
