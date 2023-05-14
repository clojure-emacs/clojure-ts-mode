; https://tonsky.me/blog/clojurefmt/#ok-what-do-we-do-then
(when something
  body)

(defn f [x]
  body)

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

; second cond is not aligned
(or (condition-a)
  (condition-b))

; or/and are the only forms where this looks not ideal
; other forms don’t win/lose much because of this change
(filter even?
  (range 1 10))

; my way is actually better if fn name is looooooooooong
(clojure.core/filter even?
  (range 1 10))

; 1 additional space, no big deal
(filter
  even?
  (range 1 10))

(:my/keyword
  {:my/keyword 1
   :another-keyword 2}
  "default value")
