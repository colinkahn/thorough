(ns thorough.runner
  (:require [cloverage.instrument :as inst]
            [cloverage.dependency :as dep]
            [cloverage.coverage :as coverage]
            [clojure.spec.test.alpha :as st]
            [clojure.string :as str]
            [clojure.pprint :refer [print-table]])
  (:gen-class))

(def ^:dynamic *covered* nil)
(def ^:dynamic *excluded-lines* nil)

(defn track-covered [line-hint form]
  ;; TODO: can we figure out how to get all line numbers
  ;; for a defn and then use that to exclude?
  (let [form' {:line line-hint
               :form form
               :file *file*
               :hits 0}
        idx (dec (count (swap! *covered* conj form')))]
    `(do
       (swap! *covered* update-in [~idx :hits] inc)
       ~form)))

(defn not-covered []
  (->> (deref *covered*)
       (filter (comp zero? :hits))
       (filter (comp not (set *excluded-lines*) :line))
       (seq)))

(defn -main [& ns-paths]
  (let [namespaces (coverage/find-nses ns-paths [])
        ordered-nses (dep/in-dependency-order (map symbol namespaces))]
    (binding [*covered* (atom [])
              *excluded-lines* [13]]
      (doseq [namespace ordered-nses]
        (println "Instrumenting" namespace)
        (inst/instrument #'track-covered namespace))

      (let [check-results (st/check (st/instrument))
            failed-check (->> check-results
                              (remove (comp true? :result :clojure.spec.test.check/ret))
                              seq)]
            (if failed-check
              (println "The following failed to check"
                       (->> failed-check
                            (map :sym)
                            (str/join ", ")))
              (println "Checked:"
               (->> check-results
                    (map :sym)
                    (str/join ", ")))))

      ;; needs more checks, like look for :result true

      (if-some [coll (not-covered)]
        (print-table (map #(select-keys % [:file :line :form]) coll))
        (println "All code covered!"))

      (System/exit 1))))
