(ns thorough.sample
  (:require [clojure.spec.alpha :as s]))

(defn foo [a b]
  (+ a b))

(defn bar [a b]
  (->> a
       (+ b)
       (- a)
       (* a b)))
