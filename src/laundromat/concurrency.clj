(ns laundromat.concurrency
  (:require [clojure.core.memoize :as memo]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [laundromat.core :refer :all]
            [clojure.math.combinatorics :as combinatorics]))

;; infrastructure

(defn distinct-by
  "like distinct, but on a particular fn"
  {:added "1.0"
   :static true}
  [g coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                 ((fn [[f :as xs] seen]
                    (when-let [s (seq xs)]
                      (let [res (g f)]
                        (if (contains? seen res)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen res)))))))
                    xs seen)))]
    (step coll #{})))

(defn find-first
  "returns the first item matching f in the coll"
  [f coll]
  (first (filter f coll)))

(defmacro debug [thing & messages]
  `(let [x# ~thing]
     (.println *out* (str "DEBUG: " (str '~thing " " ~*file* ":" ~(:line (meta &form)) " is " (pr-str ~thing) " " (if (nil? ~thing) "nil" (class ~thing))) ~@messages))
     (flush)
     ~thing))

(defmacro meh
  ([expr failing]
   `(try
      [::success ~expr]
      (catch Throwable e#
        [::failure (ex-info (.getMessage e#) {:info ~failing})]))))

(def shrinking-sentinel
  (gen/gen-pure
    [false [(gen/rose-pure true)]]))

(defn take-until [x xs]
  (reverse (drop-while #(not (= x %)) (reverse xs))))

(defn run-the-model [commands transitions]
  (reductions
    (fn [current-model-state [command-name args :as foo]]
      ((get-in transitions [command-name :next]) current-model-state args))
    ((get-in transitions [:initial-state :initial]))
    commands))

(defn run-permutations [workers-commands failing-command transitions run-commands]
  (->> (combinatorics/permutations (apply concat (vals workers-commands)))
    (map
      (fn [permuted-commands]
        (take-until failing-command permuted-commands)))
    distinct
    (map
      (fn [permuted-commands]
          (->> permuted-commands
            (map
              (fn [[command-name args :as command]]
                (if-let [resulting-actual (:resulting-actual (find-first (fn [{result-command :command}] (= command result-command)) run-commands))]
                  [resulting-actual
                   command]
                  [nil
                   command]))))))
    (map
      (fn [permuted-commands-with-results]
        (let [model-states
              (run-the-model (map second permuted-commands-with-results) transitions)]
          (map
            (fn [[resulting-actual command] model-state]
              (if resulting-actual
                (meh
                  ((get-in transitions [(first command) :postcondition] (constantly true))
                     resulting-actual
                     model-state
                     (second command))
                  {:model-state model-state
                   :resulting-actual resulting-actual
                   :run-commands run-commands
                   :permuted-commands-with-results permuted-commands-with-results
                   :model-states model-states})
                [::success true]))
            permuted-commands-with-results
            (rest model-states)))))))

(defn check-permutations [permutation-results]
  (some
    (fn [result]
      (every? #(= ::success (first %)) result))
    permutation-results))

(defn run-command-concurrently [transitions workers-commands [command-name args :as current-command] symbolic-state actual-state run-commands]
  (let [[actual-result-type resulting-actual] (meh (apply (get-in transitions [command-name :command]) actual-state args) nil)
        [symbolic-result-type [resulting-symbolic symbolic-run-commands]] (meh (dosync
                                                                                 [(alter symbolic-state #((get-in transitions [command-name :next]) % args))
                                                                                  (alter run-commands conj {:command current-command :resulting-actual resulting-actual})]) nil)]
    (if (and (= actual-result-type ::success) (= symbolic-result-type ::success))
      (let [[postcondition-success postcondition-result]
            (meh ((get-in transitions [command-name :postcondition] (constantly true)) resulting-actual resulting-symbolic args) nil)]
        (if (= postcondition-success ::success)
          {:symbolic-state resulting-symbolic
           :result resulting-actual
           :success true}
          {:failure (str "postcondition failed " postcondition-success " " postcondition-result)
           :current-command current-command
           :symbolic-state resulting-symbolic
           :actual-result resulting-actual
           :run-commands symbolic-run-commands
           :permutation-checker (fn []
                                  (run-permutations workers-commands current-command transitions @run-commands))}))
      {:failure [actual-result-type resulting-actual symbolic-result-type resulting-symbolic args]})))

(defn my-deliver [finished worker-id x test-run-n]
  (deliver (nth finished worker-id) x))

(defonce test-run-count (atom 0))

(defn run-worker [test-run-n worker-id workers-commands transitions symbolic-state actual-state finished]
  (future
    (let [run-commands (ref [])]
      (try
        (reduce
          (fn [{is-finished :finished result :result :as r} current-command]
            (if is-finished
              r
              (let [result (run-command-concurrently transitions workers-commands current-command symbolic-state actual-state run-commands)]
                (if (:success result)
                  (assoc r :result result)
                  (do
                    (my-deliver finished worker-id result test-run-n)
                    (assoc r :finished true))))))
          {:finished false}
          (workers-commands worker-id))
        (my-deliver finished worker-id true test-run-n)
        (catch Throwable e
          (.printStackTrace e)
          (my-deliver finished worker-id e test-run-n))))))

(defn run-and-check-commands-concurrently [workers-commands transitions concurrency]
  (let [test-run-n      (swap! test-run-count inc)
        finished        (into [] (map (fn [_] (promise)) (range concurrency)))
        symbolic-state  (ref ((get-in transitions [:initial-state :initial])))
        actual-state    ((get-in transitions [:initial-state :subject]))]
    (dotimes [worker-id concurrency]
      (run-worker test-run-n worker-id workers-commands transitions symbolic-state actual-state finished))
    (doall
      (->>
        (map
          (fn [n x]
            (deref n 300 (ex-info "timed out waiting for test worker" {:worker-id x})))
          finished
          (range concurrency))
        doall
        (map (fn [r]
               (if (= true r)
                 r
                 (if (isa? Exception r)
                   (throw r)
                   (let [checker (:permutation-checker r)
                         checker-results (into [] (distinct-by #(map (fn [[_ ^Throwable t]] (.getMessage t)) %) (map #(filter (fn [[r _]] (= r ::failure)) %) (checker))))]
                     (if (check-permutations checker-results)
                       true
                       (assert false checker-results)))))))))
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
          (run-and-check-commands-concurrently workers-commands transitions concurrency) nil)]
    (if (and is-shrinking (= result-type ::success))
      (run-until-failure-or-stop
        times-to-repeat-for-races
        (fn []
          (meh
            (run-and-check-commands-concurrently workers-commands transitions concurrency) nil)))
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
                             [n
                              (gen/fmap
                                (fn [commands]
                                  (map
                                    (fn [[command-name args] command-sequence-number]
                                      [command-name args {:worker-id n :command-seq command-sequence-number}])
                                    commands
                                    (iterate inc 0)))
                                (generate-commands transitions))])
                           (range concurrency))))))))

(defn run-state-machine-concurrent
  ([transitions] (run-state-machine-concurrent transitions (gen/choose 1 2)))
  ([transitions concurrency-gen]
   (prop/for-all [workers-commands (generate-worker-commands transitions concurrency-gen)
                  is-shrinking shrinking-sentinel]
                 (let [r (run-and-repeat-for-races (:commands workers-commands) transitions (:concurrency workers-commands) is-shrinking)]
                   r))))
