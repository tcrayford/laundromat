(ns laundromat.core-test
  (:require [clojure.test :refer :all]
            [laundromat.core :refer :all]
            [laundromat.concurrency :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(defn empty-ticker! [] (atom 0))

(defn take-ticker! [ticker amount]
  (let [r @ticker]
    (reset! ticker (+ r amount))
    (+ r amount)))

(defn reset-ticker! [ticker]
  (swap! ticker (constantly 0)))

(def ticker-machine
  {:initial-state  {:initial  (constantly 0)
                    :subject  empty-ticker!}
   :take-ticket  {:next  (fn  [previous-state args]  (+ previous-state (first args)))
                  :args  (gen/tuple gen/s-pos-int)
                  :command take-ticker!
                  :postcondition (fn [result state args]
                                   (assert (= result state) (str "expected " result " to equal " state)))}
   :reset         {:next  (constantly 0)
                   :command #(reset-ticker! %)}})


(defspec state-machine-test
  1000
  (run-state-machine-concurrent ticker-machine))
