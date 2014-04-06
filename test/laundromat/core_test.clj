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
  #_(let [r (deref ticker)]
    (reset! ticker (+ r amount))
    (+ r amount))

  ;; currently, no race condition (in theory ;))
  (swap! ticker (fn [original] (+ original amount))))

(defn reset-ticker! [ticker]
  (reset! ticker 0))

(def ticker-machine
  {:initial-state  {:initial  (constantly 0)
                    :subject  empty-ticker!}

   :take-ticket  {:next  (fn  [previous-state args]
                           (+ previous-state (first args)))
                  :args  (gen/tuple (gen/return 1))
                  :command take-ticker!
                  :postcondition (fn [result state args]
                                   (assert (= result state) (str "expected ticket " result " to equal model " state)))}

   ;:reset         {:next  (constantly 0)
   ;                :command reset-ticker!}
   })

(defspec state-machine-test
  1000
  (run-state-machine-concurrent ticker-machine (gen/return 2)))

(deftest run-the-model-test
  (is (= 1
         (run-the-model
           [[:inc []]]
           {:initial-state {:initial (constantly 0)}
            :inc {:next (fn [model-state args] (inc model-state))}}))))

(deftest check-permutations-test
  (is
    (= true
       (check-permutations
         {0 [[:inc []]]
          1 [[:inc []]]}
         [:inc []]
         {:initial-state {:initial (constantly 0)}
          :inc {:next (fn [model-state args] (inc model-state))
                :postcondition (fn [result state _]
                                 (assert (= result state)))}}
         2))))

(defspec take-until-test
  (prop/for-all [ys (gen/bind (gen/not-empty (gen/vector gen/int))
                              (fn [ys]
                                (gen/hash-map :x (gen/return (rand-nth ys))
                                              :xs (gen/return ys))))]
                (let [{x :x xs :xs} ys
                      result (take-until x xs)]
                  (some
                    (fn [[x y]]
                      (= x y))
                    (map vector
                         xs
                         result)))))
