(ns laundromat.concurrency
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [laundromat.core :refer :all]))

(defmacro meh [expr]
  `(try
     [::success ~expr]
     (catch Throwable e#
       [::failure e#])))

(def shrinking-sentinel
  (gen/gen-pure
    [false [(gen/rose-pure true)]]))

(defn run-command-concurrently [transitions [command-name args] symbolic-state actual-state]
  (let [[actual-result-type resulting-actual] (meh (apply (get-in transitions [command-name :command]) actual-state args))
        [symbolic-result-type resulting-symbolic] (meh (swap! symbolic-state #((get-in transitions [command-name :next]) % args)))]
    (if (and (= actual-result-type ::success) (= symbolic-result-type ::success))
      (let [[postcondition-success postcondition-result]
            (meh ((get-in transitions [command-name :postcondition] (constantly true)) resulting-actual resulting-symbolic args))]
        (if (= postcondition-success ::success)
          {:symbolic-state resulting-symbolic
           :result resulting-actual
           :success true}
          (do
            (.println *out* (str postcondition-result))
            {:failure (str "postcondition failed " postcondition-success " " postcondition-result)})))
      {:failure [actual-result-type resulting-actual symbolic-result-type resulting-symbolic args]})))

(defn my-deliver [finished worker-id x test-run-n]
  (deliver (nth finished worker-id) x))

(defonce test-run-count (atom 0))

(defn run-worker [test-run-n worker-id workers-commands transitions symbolic-state actual-state finished]
  (future
    (try
      (reduce
        (fn [{is-finished :finished result :result :as r} current-command]
          (if is-finished
            r
            (let [result (run-command-concurrently transitions current-command symbolic-state actual-state)]
              (if (:success result)
                (assoc r :result nil)
                (do
                  (.println *out* (str result " " test-run-n))
                  (my-deliver finished worker-id result test-run-n)
                  (assoc r :finished true))))))
        {:finished false}
        (workers-commands worker-id))
        (my-deliver finished worker-id true test-run-n)
      (catch Exception e
        (.printStackTrace e)
        (my-deliver finished worker-id e test-run-n)))))

(defn run-and-check-commands-concurrently [workers-commands transitions concurrency]
  (let [test-run-n      (swap! test-run-count inc)
        finished        (into [] (map (fn [_] (promise)) (range concurrency)))
        symbolic-state  (atom ((get-in transitions [:initial-state :initial])))
        actual-state    ((get-in transitions [:initial-state :subject]))]
    (dotimes [worker-id concurrency]
      (run-worker test-run-n worker-id workers-commands transitions symbolic-state actual-state finished))
    (doall
      (map
        (fn [n x]
          (let [r (deref n 300 [::timeout x])]
            (if (= true r)
              r
              (if (isa? Exception r)
                (throw r)
                (assert false r)))))
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
        (if (= left 0)
          (if (= result-type ::success)
            actual-result
            (throw actual-result)))))))

(defn run-and-repeat-for-races [workers-commands transitions concurrency is-shrinking]
  (let [[result-type actual-result]
        (meh
          (run-and-check-commands-concurrently workers-commands transitions concurrency))]
    (if (and is-shrinking (= result-type ::success))
      (run-until-failure-or-stop
        times-to-repeat-for-races
        (fn []
          (meh
            (run-and-check-commands-concurrently workers-commands transitions concurrency))))
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
