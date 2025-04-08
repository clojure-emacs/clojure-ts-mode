(ns spec
  (:require
   [clojure.spec.alpha :as s]))

(s/def ::username string?)
(s/def ::age number?)
(s/def ::email string?)
