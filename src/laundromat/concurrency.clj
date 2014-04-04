(ns laundromat.concurrency
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [laundromat.core :refer :all]))

(def shrinking-sentinel
  (gen/gen-pure
    [false [(gen/rose-pure true)]]))

(defn debug [& args]
  (.println *out*
            (apply str args)))

(defn run-command-concurrently [transitions [command-name args] symbolic-state actual-state]
  (let [[actual-result-type resulting-actual] (try [::success (apply (get-in transitions [command-name :command]) actual-state args)]
                 (catch Exception e
                   [::failure e]))
        [symbolic-result-type resulting-symbolic] (try [::success (swap! symbolic-state #((get-in transitions [command-name :next]) % args))]
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

(defn my-deliver [finished worker-id x test-run-n]
  (debug (str "about to deliver on " test-run-n "/" worker-id " " x))
  (deliver (nth finished worker-id) x))

(defonce test-run-count (atom 0))

(defn run-and-check-commands-concurrently [workers-commands transitions concurrency]
  (let [test-run-n      (swap! test-run-count inc)
        finished        (map (fn [_] (promise)) (range concurrency))
        symbolic-state  (atom ((get-in transitions [:initial-state :initial])))
        actual-state    ((get-in transitions [:initial-state :subject]))]
    (dotimes [worker-id concurrency]
      (future
        (debug "starting work on " test-run-n "/" worker-id)
        (try
          (loop [current-command (first (workers-commands worker-id))
                 remaining       (rest  (workers-commands worker-id))]
            (debug "running command " current-command " on " (str test-run-n "/" worker-id))
            (if current-command
              (let [result (run-command-concurrently transitions current-command symbolic-state actual-state)]
                (if (:success result)
                  (recur (first remaining)
                         (rest remaining))
                  (my-deliver finished worker-id result test-run-n)))
              (my-deliver finished worker-id true test-run-n)))
          (catch Exception e
            (debug e)
            (my-deliver finished worker-id e test-run-n)))
        (debug "finished on " test-run-n "/" worker-id)))
    (doall
      (map
        (fn [n x]
          (println "about to deref on " test-run-n "/" x n)
          (let [r (deref n 300 ::timeout)]
            (if (= true r)
              r
              (if (isa? Exception r)
                (throw r)
                (assert false (str r " " test-run-n "/" x))))))
        finished
        (range concurrency)))
    true))

(def times-to-repeat-for-races 10)

(defn run-until-failure-or-stop [times f]
  (loop [left times]
    (let [[result-type actual-result] (f)]
      (if (and (= result-type ::success)
               (not (= left 0)))
        (recur (dec left))
        (throw actual-result)))))

(defn run-and-repeat-for-races [workers-commands transitions concurrency is-shrinking]
  (let [[result-type actual-result]
        (try
          [::success (run-and-check-commands-concurrently workers-commands transitions concurrency)]
          (catch Exception e
            [::failure e]))]
    (if (and is-shrinking (= result-type ::success))
      (run-until-failure-or-stop
        times-to-repeat-for-races
        (fn []
          (try
            [::success (run-and-check-commands-concurrently workers-commands transitions concurrency)]
            (catch Exception e
              [::failure e]))))
      (if (= result-type ::success)
        true
        (if (isa? Exception actual-result)
          (throw actual-result)
          (assert false actual-result))))))

(defn generate-worker-commands [transitions concurrency-gen]
  (gen/bind
    concurrency-gen
    (fn [concurrency]
      (gen/hash-map
        :concurrency
        (gen/return concurrency)
        :commands
        (apply gen/hash-map
               (apply concat
                      (map (fn [n]
                             [n (generate-commands transitions)])
                           (range concurrency))))))))

(defn run-state-machine-concurrent
  ([transitions] (run-state-machine-concurrent transitions (gen/choose 1 (.. Runtime getRuntime availableProcessors))))
  ([transitions concurrency-gen]
   (prop/for-all [workers-commands (generate-worker-commands transitions concurrency-gen)
                  is-shrinking shrinking-sentinel]
                 (run-and-repeat-for-races (:commands workers-commands) transitions (:concurrency workers-commands) is-shrinking))))
