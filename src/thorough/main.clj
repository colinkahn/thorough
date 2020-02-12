(ns thorough.main
  (:require [clojure.edn :as edn]
            [thorough.check :as check]
            [clojure.pprint :as pprint]
            [clojure.string :as s])
  (:gen-class))

(defn format-lines [lines]
  (s/join ", " (set lines)))

(defn -main [config-file]
  (let [{:keys [ns-paths file->ns]} (edn/read-string (slurp config-file))
        summary (map (fn [{::check/keys [sym ret hit miss]}]
                       {"Symbol" sym
                        "Check"  (-> ret :clojure.spec.test.check/ret :result)
                        "Hit"    (format-lines (map :line hit))
                        "Miss"   (format-lines (map :line miss))})
                     (check/check ns-paths file->ns))]
    (pprint/print-table summary))
  (System/exit 0))
