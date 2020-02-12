(ns thorough.sample
  (:require [clojure.spec.alpha :as s]))

(defn odd-adder [a b]
  (if (odd? a)
    (+ a b)
    (- a b)))

(defn with-odd-adder [a b]
  (odd-adder a b))

(defn io-boundry! []
  (throw (ex-info "HIT BOUNDRY!" {})))

(defn calls-boundry [x y]
  (io-boundry!))
