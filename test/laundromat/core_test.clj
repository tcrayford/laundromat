(ns laundromat.core-test
  (:require [clojure.test :refer :all]
            [laundromat.core :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn empty-ticker! [] (atom 0))

(defn take-ticker! [ticker amount] (swap! ticker #(+ % amount)))

(defn reset-ticker! [ticker] (swap! ticker (constantly 0)))

(def transitions
  {:initial-state  {:initial  (constantly 0)
                    :subject  empty-ticker!}
   :take-ticket  {:next  (fn  [previous-state args]  (+ previous-state (first args)))
                  :args  (gen/tuple gen/s-pos-int)
                  :command take-ticker!
                  :postcondition (fn [result state args] (assert (= result state) (str "expected " result " to equal " state)))}
   :reset         {:next  (constantly 0)
                   :command #(reset-ticker! %)}})

(defn run-command [transitions [command-name args] symbolic-state actual-state]
  (let [[actual-result-type resulting-actual] (try [::success (apply (get-in transitions [command-name :command]) actual-state args)]
                 (catch Exception e
                   [::failure e]))
        [symbolic-result-type resulting-symbolic] (try [::success ((get-in transitions [command-name :next]) symbolic-state args)]
                                                    (catch Exception e
                                                      [::failure e]))]
    (if (and (= actual-result-type ::success) (= symbolic-result-type ::success))
      (let [[postcondition-success postcondition-result]
            (try
              [::success ((get-in transitions [command-name :postcondition] (constantly true)) resulting-actual resulting-symbolic args)]
              (catch Exception e
                [::failure e]))]
        (if (= postcondition-success ::success)
          {:symbolic-state resulting-symbolic
           :result resulting-actual
           :success true}
        {:failure (str "postcondition failed " postcondition-success " " postcondition-result)}))
      {:failure [resulting-actual resulting-symbolic]})))

(defn run-and-check-commands [commands transitions]
  (loop [current-command (first commands)
         remaining (rest commands)
         symbolic-state  ((get-in transitions [:initial-state :initial]))
         actual-state    ((get-in transitions [:initial-state :subject]))]
    (if current-command
      (let [result (run-command transitions current-command symbolic-state actual-state)]
        (if (:success result)
          (recur (first remaining)
                 (rest remaining)
                 (:symbolic-state result)
                 actual-state)
          (assert false result)))
      true)))

(defn gen-transition [transitions]
  (gen/bind
    (gen/elements (keys (dissoc transitions :initial-state)))
    (fn [transition-name]
      (gen/tuple (gen/return transition-name)
                 (or (get-in transitions [transition-name :args]) (gen/return []))))))

(defn preconditions-all-satisfied? [transitions commands]
  (loop [current-command (first commands)
         remaining (rest commands)
         symbolic-state ((get-in transitions [:initial-state :initial]))]
    (if current-command
      (let [[command-name args] current-command
            [precondition-result precondition-success]
            (try [::success ((get-in transitions [command-name :precondition] (constantly true)) symbolic-state args)]
              (catch Exception e
                [::failure e]))]
        (if (and (= precondition-result ::success) precondition-success)
          (let [[symbolic-result-type resulting-symbolic]
                (try [::success ((get-in transitions [command-name :next] (constantly true)) symbolic-state args)]
                  (catch Exception e
                    [::failure e]))]
            (if (= symbolic-result-type ::success)
              (recur
                (first remaining)
                (rest remaining)
                resulting-symbolic)
              false))
          false))
      true)))

(defn generate-commands
  "generates commands from a transitions map"
  [transitions]
  (gen/such-that #(preconditions-all-satisfied? transitions %)
                 (gen/list (gen-transition transitions))))


(defn run-state-machine [transitions]
  (prop/for-all [commands (generate-commands transitions)]
                (run-and-check-commands commands transitions)))

(defspec state-machine-test
  100
  (run-state-machine transitions))
