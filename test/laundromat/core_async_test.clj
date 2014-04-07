(ns laundromat.core-async-test
  (:require [clojure.core.async :as async]
            [laundromat.core :refer :all]
            [laundromat.concurrency :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]))

(defn put [{loopback-chan :chan} i]
  (async/put! loopback-chan i (constantly true) false))

(def put-machine
  {:initial-state {:initial (constantly :stopped)
                   :subject async/chan}

   :put           {:next (constantly true)
                   :args (gen/tuple gen/int)
                   :command (fn [c i]
                              (deref
                                (future
                                  (put c i)
                                  true)
                                100
                                ::timeout))
                   :postcondition (fn [result state args]
                                    (assert (= result true)))}})

#_(defspec async-put-never-blocks
  100
  (run-state-machine-concurrent put-machine))
