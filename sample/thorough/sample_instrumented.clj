(ns thorough.sample-instrumented
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [thorough.sample :as sample]
            thorough.sample-specs))

(defn pre-calls-boundry []
  (st/instrument `sample/io-boundry!
                 {:stub #{`sample/io-boundry!}
                  :spec {`sample/io-boundry!
                         (s/fspec :args (s/cat)
                                  :ret #{:io/ret})}})
  {:gen {}})

(defn post-calls-boundry []
  (st/unstrument `sample/io-boundry!))
