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

#:hello {:world          true
         :foo            "bar"
         :some-very-long "value"}

{:name "Roma"
 :foo true}

(reify
  java.io.FileFilter
  (accept [this f]
    (.isDirectory f))

  (hello [world]
    false))

(defmulti which-color-mm (fn [m & args] (:color m)))
(defmethod which-color-mm :blue
  ([m] (print m))
  ([m f] (f m)))

(letfn [(twice [x]
          (* x 2))
        (six-times [y]
          (* (twice y) 3))]
  (println "Twice 15 =" (twice 15))
  (println "Six times 15 =" (six-times 15)))

(let [p (proxy [java.io.InputStream] []
          (read
            ([] 1)
            ([^bytes bytes] 2)
            ([^bytes bytes off len] 3)))]
  (println (.read p))
  (println (.read p (byte-array 3)))
  (println (.read p (byte-array 3) 0 3)))

(defprotocol Fly
  "A simple protocol for flying"
  (fly [this]
    "Method to fly"))

(defn foo
  ^{:bla "meta"}
  [arg]
  body)

(if ^boolean (= 2 2)
  true
  false)

(when-not true
  (println "Hello world"))

(extend-protocol prepare/SettableParameter
  clojure.lang.IPersistentMap
  (set-parameter [])
  (set-parameter [m ^PreparedStatement s i]
    (.setObject s i (->pgobject m))))
