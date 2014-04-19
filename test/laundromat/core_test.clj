(ns laundromat.core-test
  (:require [clojure.test :refer :all]
            [laundromat.core :refer :all]
            [clojure.set :as set]
            [laundromat.concurrency :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defprotocol Ticker
  (take-ticker! [ticker amount])
  (reset-ticker! [ticker]))

(deftype RacyTicker [^{:unsynchronized-mutable true} ticker]
  Ticker
  (take-ticker! [this amount]
    (let [r ticker]
      (set! ticker (+ r amount))
      (+ r amount)))

  (reset-ticker! [this]
    (set! ticker 0)))

(deftype AtomicTicker [ticker]
  Ticker
  (take-ticker! [this amount]
    (swap! ticker #(+ % amount)))
  (reset-ticker! [this]
    (reset! ticker 0)))

(defn broken-ticker [] (RacyTicker. 0))
(defn fixed-ticker [] (AtomicTicker. (atom 0)))

(def ticker-machine
  {:initial-state  {:initial  (constantly 0)
                    :subject  fixed-ticker}

   :take-ticket  {:next  (fn  [previous-state args]
                           (+ previous-state (first args)))
                  :args  (gen/tuple (gen/return 1))
                  :command take-ticker!
                  :postcondition (fn [result state args]
                                   (assert (= result state) (str "expected ticket " result " to equal model " state)))}

   :reset         {:next  (constantly 0)
                   :command reset-ticker!}})

(defspec state-machine-test
  100
  (run-state-machine ticker-machine))

(defspec concurrent-state-machine-test
  100
  (run-state-machine ticker-machine))

(defspec take-until-is-always-a-prefix
  (prop/for-all [ys (gen/bind (gen/not-empty (gen/vector gen/int))
                              (fn [ys]
                                (gen/hash-map :x (gen/return (rand-nth ys))
                                              :xs (gen/return ys))))]
                (let [{x :x xs :xs} ys
                      result (take-until x xs)]
                  (every?
                    (fn [[x y]]
                      (= x y))
                    (map vector
                         xs
                         result)))))

(defspec halve-always-concats-to-original-seq
  (prop/for-all [ys (gen/not-empty (gen/vector gen/int))]
                (assert
                  (= ys
                     (apply concat (halve ys)))
                  (str
                    "expected "
                    (into []
                          (apply concat (halve ys)))
                    " to equal "
                    ys))
                true))

(defspec halve-always-produces-two-seqs
  (prop/for-all [ys (gen/not-empty (gen/vector gen/int))]
                (assert
                  (>= 2
                     (count (halve ys)))
                  (str
                    "expected "
                    (count (halve ys))
                    " to be 2 >="))
                true))

(deftest interleavings-test
  (testing "with two empty seqs, it is empty"
    (is (= [[]]
           (interleavings [[] []]))))
  (testing "with one seq with one elem, has one interleaving"
    (is (= [[1]]
           (interleavings [[1]]))))

  (testing "with two seqs with one elem, has interleavings"
    (is (= [[1 2] [2 1]]
           (interleavings [[1] [2]])))))

(defspec interleavings-spec
  ;; have to limit vector size here to make sure we don't take aaages
  (prop/for-all [ys (gen/not-empty (gen/vector gen/int 2 15))]
                (let [xs (into #{} ys)]
                  (assert
                    (every? #(empty? (set/difference % xs))
                            (map #(into #{} %) (interleavings (halve xs)))))
                  true)))

(deftest shrink-concurrent-commands-test
  (is (=
        (shrink-concurrent-commands {:sequential [[:take-ticket [1]]] :concurrent [[[:take-ticket [1]]] [[:take-ticket [1]]]]})
        '({:sequential ()
           :concurrent [[[:take-ticket [1]]] [[:take-ticket [1]]]]}
          {:sequential [[:take-ticket [1]]] :concurrent ()}
          {:sequential [[:take-ticket [1]]] :concurrent ()}
          {:sequential ([:take-ticket [1]] [:take-ticket [1]]) :concurrent ()}
          {:sequential ([:take-ticket [1]] [:take-ticket [1]]) :concurrent ()}))))
