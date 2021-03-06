(ns thorough.sample-specs
  (:require [clojure.spec.alpha :as s]
            [thorough.sample :as sample]))

(s/fdef sample/odd-adder
        :args (s/cat :a pos-int? :b pos-int?)
        :ret int?)

(s/fdef sample/with-odd-adder
        :args (s/cat :a pos-int? :b pos-int?)
        :ret int?)

(s/fdef sample/calls-boundry
        :args (s/cat :x pos-int? :y pos-int?)
        :ret #{:io/ret})
