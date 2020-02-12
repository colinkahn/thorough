(ns thorough.core
  (:require [cloverage.instrument :as inst]
            [cloverage.coverage :as coverage]
            [cloverage.report :as rep]
            [cloverage.report.raw :as raw]
            [cloverage.report.html :as html]
            [cloverage.report.console :as console]
            ;; spec
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]))

(binding [coverage/*covered* (atom [])
          coverage/*instrumented-ns* 'thorough.core
          *file* "foo"
          *source-path* "foo"]
  (let [forms '((defn foo [a b] (+ a b))
                (defn bar [a b] (- a b))
                (foo 1 2))]
    (doseq [i (range (count forms))]
      (eval (inst/wrap #'coverage/track-coverage i (nth forms i))))
    ;; generate reports
    (let [forms (rep/gather-stats (coverage/covered))]
      (html/report (java.io.File. "reports") forms)
      #_(raw/report (java.io.File. "reports") forms @coverage/*covered*)
      #_(console/summary forms 50 80))))

;; POC

(def ^:dynamic *covered* nil)

(defn track-visited [line-hint form]
  (let [form' {:line line-hint :form form :hits 0}
        idx (dec (count (swap! *covered* conj form')))]
    `(do
       (swap! *covered* update-in [~idx :hits] inc)
       ~form)))

(binding [*covered* (atom [])]
  (let [forms '((defn foo [a b] (+ a b))
                (defn bar [a b] (- a b))
                (foo 1 2))]
    (doseq [i (range (count forms))]
      (eval (inst/wrap #'track-visited i (nth forms i))))
    ;; 
    (filter (comp zero? :hits) (deref *covered*))))

;; More complete version

(binding [*covered* (atom [])]
  (let [forms '((defn odd-adder [a b]
                  (if (odd? a)
                    (+ a b)
                    (- a b))))]
    (doseq [i (range (count forms))]
      (println (eval (inst/wrap #'track-visited i (nth forms i)))))

    (st/check-fn thorough.core/odd-adder
                 (s/fspec :args (s/cat :a #{1} :b pos-int?)
                          :ret int?))
    ;; 
    (filter (comp zero? :hits) (deref *covered*))))


(coverage/find-nses ["src"] [#"^thorough.sample$"])

(binding [*covered* (atom [])]
  (inst/instrument #'track-visited 'thorough.sample)
  (st/check-fn thorough.sample/odd-adder
               (s/fspec :args (s/cat :a pos-int? :b pos-int?)
                        :ret int?))
  (st/check-fn thorough.sample/with-odd-adder
               (s/fspec :args (s/cat :a pos-int? :b pos-int?)
                        :ret int?))
  (filter (comp zero? :hits) (deref *covered*)))

(require 'thorough.sample :reload-all)

(apply thorough.sample/odd-adder [1 2])
(st/unstrument)

;; 
(binding [*covered* (atom [])]
  (inst/instrument #'track-visited 'thorough.sample)
  (inst/instrument #'track-visited 'thorough.sample-specs)

  (st/check)

  (->> (deref *covered*)
       (filter (comp zero? :hits))
       (map :line)))
