(ns thorough.check
  (:require [clojure.spec.test.alpha :as st]
            [cloverage.dependency :as dep]
            [cloverage.coverage :as coverage]
            [thorough.instrument :as inst]))

(def ^:dynamic *covered* (atom []))
(def ^:dynamic *check-sym* ::eval)

(defn track-covered [ctx form]
  (let [form' {:line (:line ctx)
              :form form
              :file *file*
              :defn-sym (when-some [s (:defn-sym ctx)] (symbol s))
              :hits {}}
        idx (dec (count (swap! *covered* conj form')))]
    `(do
       (swap! *covered* update-in [~idx :hits *check-sym*] (fnil inc 0))
       ~form)))

(defn check-passed? [r]
  (-> r :clojure.spec.test.check/ret :result true?))

(defn qualify-sym [{:as opts :keys [::check-sym ::file->ns defn-sym file]}]
  (when defn-sym
    (if-some [file-ns (file->ns file)]
      (symbol (name file-ns) (name defn-sym))
      (throw (ex-info (str "No namespace found for file " file) opts)))))

(defn missing [covered {:as opts :keys [::check-sym]}]
  (->> covered
       (filter #(#{check-sym} (qualify-sym (merge opts %))))
       (filter (comp zero? #(get % ::eval 0) :hits))
       (filter (comp zero? #(get % check-sym 0) :hits))
       (seq)))

(defn hits [covered {:as opts :keys [::check-sym]}]
  (->> covered
       (filter #(#{check-sym} (qualify-sym (merge opts %))))
       (filter (some-fn (comp (complement zero?) #(get % ::eval 0) :hits)
                        (comp (complement zero?) #(get % check-sym 0) :hits)))
       (seq)))

(defn check
  "Given ns-paths and file->ns map instruments and checks all functions with
  registered fdefs. Returns a report for each including the sym checked, check results
  and any lines not hit during the check."
  [ns-paths file->ns]
  (let [namespaces (coverage/find-nses ns-paths [])
        ordered-nses (dep/in-dependency-order (map symbol namespaces))]

    (binding [*covered* (atom [])]
      (doseq [namespace ordered-nses]
        (inst/instrument #'track-covered namespace))

      (let [check-syms (st/instrument)
            check-rets (atom {})]
        (doseq [check-sym check-syms]
          (binding [*check-sym* check-sym]
            (swap! check-rets assoc check-sym (first (st/check check-sym)))))

        (let [covered *covered*]
          (if-not (some ::eval (mapv :hits @covered))
            (throw (ex-info "No hits after check!" {:covered @covered})))

          (map (fn [check-sym]
                 {::sym check-sym
                  ::ret (get @check-rets check-sym)
                  ::hit (hits @covered {::check-sym check-sym ::file->ns file->ns})
                  ::miss (missing @covered {::check-sym check-sym ::file->ns file->ns})})
               check-syms))))))

(comment
  (check ["sample"] {"thorough/sample.clj" 'thorough.sample}))
