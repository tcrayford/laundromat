(ns laundromat.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import java.util.concurrent.LinkedBlockingQueue))

;; infrastructure

;; run

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

;; generation

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
                 (gen/vector (gen-transition transitions) 0 16)))

(defn run-state-machine [transitions]
  (prop/for-all [commands (generate-commands transitions)]
                (run-and-check-commands commands transitions)))
