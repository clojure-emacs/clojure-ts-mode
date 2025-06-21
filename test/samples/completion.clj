(ns completion
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest testing is]])
  (:import
   (java.time Instant LocalDate)))

(def my-var "Hello")
(def my-another-var "World")

(defn- my-function
  "This is a docstring."
  [some-arg]
  (let [to-print (str "Hello" some-arg)]
    (println my-var my-another-var to-print)))

(fn [anon-arg]
  anon-arg)

(def hello-string "Hello")

(defn complete-example
  "Docstring won't interfere with completion."
  [arg1 arg2 & {:keys [destructured]}]
  ;; Here only function args and globals should be completed.
  (println arg1 arg2 destructured)
  (let [foo                       "bar" ; comment
        baz                       ^String hello
        map-var                   {:users/usename "Roma"}
        {:users/keys [username]}  map-var
        another-map               {:address "Universe"}
        {custom-address :address} another-map
        bar                       :kwd]
    ;; Here let bindings are available in addition to globals and function args.
    (println arg1 foo map-var custom-address username)
    (when-let [nested-var "Whatever"]
      (with-open [output-stream (io/output-stream "some-file")]
        (println foo
                 baz
                 hello
                 map-var
                 username
                 another-map
                 custom-address
                 bar)
        ;; Here we should see everything
        (output-stream nested-var output-stream another-map)))
    ;; And here only let bindings, globals and function args again.
    (println username)))

(def vec-variable ["one" "two" "three"])

(let [[one two three] vec-variable]
  (println one two three))

(defn nested-fn
  [top-arg]
  (filter (fn [item]
            ;; Both arguments are available here.
            (= item top-arg))
          [1 2 3 4 5]))

;; Works for top-level bindings and for nested `:let` bindings.
(for [digit vec-variable
      :let  [prefixed-digit (str "hello-" digit)]]
  (println prefixed-digit digit))

;; Same for `doseq`
(doseq [word vec-variable
        :let  [suffixed-word (str "hello-" word)]]
  (println suffixed-word word))

;; Can complete any keyword from the buffer
(do :users/usename
    :address
    :kwd)
