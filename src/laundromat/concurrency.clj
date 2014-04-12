(ns laundromat.concurrency
  (:require [clojure.core.memoize :as memo]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [laundromat.core :refer :all]
            [clojure.math.combinatorics :as combinatorics]))

;; TODO:
;; use namespaced keywords in the state-machine map so it's clear what refers to what
;; :model/initial
;; :actual/initial
;; :model/next
;; :actual/next
;;
;; weighting of command generation
;;
;; set the timeout yourself

;; infrastructure

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

(defn exclude-nth
  "Exclude the nth value in a collection."
  [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (zero? n)
        (rest coll)
        (cons (first s)
              (exclude-nth (dec n) (rest s)))))))

;; core
(defn interleavings
  ([xs] (interleavings (first xs) (second xs) '(())))
  ([first-commands second-commands out]
   (if (and (empty? first-commands)
            (empty? second-commands))
     out
     (lazy-cat
       (if (first first-commands)
         (map #(cons (first first-commands) %) (interleavings (rest first-commands) second-commands out)))
       (if (first second-commands)
         (map #(cons (first second-commands) %) (interleavings first-commands (rest second-commands) out)))))))

(defn halve [xs]
  (split-at (/ (count xs) 2) xs))

;; generator
(defn all-interleavings-ok? [state-machine sequential-prefix commands]
  (every? #(preconditions-all-satisfied? state-machine (concat sequential-prefix %))
          (interleavings (halve commands))))

(defn remove-sequential [generated-commands]
  (let [{:keys [sequential concurrent]} generated-commands]
    (map-indexed
      (fn [index _]
        {:sequential (exclude-nth index sequential)
         :concurrent concurrent})
      sequential)))

(defn remove-concurrent [generated-commands]
  (let [{:keys [sequential concurrent]} generated-commands]
    (lazy-cat
      (map-indexed
        (fn [index _]
          {:sequential sequential
           :concurrent (exclude-nth index (first concurrent))})
        (first concurrent))
      (map-indexed
        (fn [index _]
          {:sequential sequential
           :concurrent (exclude-nth index (second concurrent))})
        (second concurrent)))))

(defn move-concurrent->sequential [generated-commands]
  (let [{:keys [sequential concurrent]} generated-commands]
    (lazy-cat
      (map-indexed
        (fn [index _]
          {:sequential (concat sequential [(nth (first concurrent) index)])
           :concurrent (exclude-nth index (first concurrent))})
        (first concurrent))
      (map-indexed
        (fn [index _]
          {:sequential (concat sequential [(nth (second concurrent) index)])
           :concurrent (exclude-nth index (second concurrent))})
        (second concurrent)))))

(defn shrink-concurrent-commands [generated-commands]
  (lazy-cat
    (remove-sequential generated-commands)
    (remove-concurrent generated-commands)
    (move-concurrent->sequential generated-commands)))

(defn shrink-tree-concurrent-commands [generated-commands]
  [generated-commands
   (map shrink-tree-concurrent-commands (shrink-concurrent-commands generated-commands))])

;; runner

(defn run-commands-concurrently [commands state-machine]
  (let [worker-1-finished (promise)
        worker-2-finished (promise)]

    )
  ;; run the sequential prefix (checking postconditions)
  ;; for each half of the concurrent suffix:
  ;;   run a worker that:
  ;;     runs each command in turn
  ;;     records the actual return value after each command
  ;;     delivers a result as a postcondition fails
  ;;     otherwise delivers true when it's finished
  ;; for each worker:
  ;;   wait for the result, timing out after 300ms
  ;; map over each worker:
  ;;   if it fails, return the results
  ;;   if it passes, return `true`
  ;; check that each worker passed
  ;;
  )

(defn failing? [results]
  true)

(defn retry-until-failure [])

(defn check-interleaved-postconditions [])

(defn check-concurrent-run [])

(defn run-and-check-sequential-commands [commands state-machine]
  (loop [current-command (first commands)
         remaining (rest commands)
         symbolic-state  ((get-in state-machine [:initial-state :initial]))
         actual-state    ((get-in state-machine [:initial-state :subject]))]
    (if current-command
      (let [result (run-command state-machine current-command symbolic-state actual-state)]
        (if (:success result)
          (recur (first remaining)
                 (rest remaining)
                 (:symbolic-state result)
                 actual-state)
          (assert false result)))
      {:symbolic-state symbolic-state
       :actual-state   actual-state})))

;; api

(defn gen-concurrenct-commands [state-machine]
  (gen/bind (gen/vector (gen-transition state-machine) 0 16)
    (fn [sequential-prefix]
      (gen/bind
        (gen/such-that #(all-interleavings-ok? state-machine sequential-prefix %)
                       (gen/vector (gen-transition state-machine) 2 16))
        (fn [concurrent-suffix]
          (let [generated
                {:sequential sequential-prefix
                 :concurrent (halve concurrent-suffix)}]
          (gen/gen-pure
            (shrink-tree-concurrent-commands generated))))))))

(defn run-state-machine-concurrent
  "run a state machine concurrently and check that postconditions are satisfied
  in some interleaving of commands"
  [state-machine]
  (prop/for-all [commands (gen-concurrenct-commands state-machine)
                 is-shrinking shrinking-sentinel]
                ;; run commands concurrently once
                ;; if we're shrinking and they pass, retry up to 10 times
                ;; if they fail, check if they still fail with interleavings
                (let [results (run-commands-concurrently commands state-machine)]
                  (if (and (failing? results) is-shrinking)
                    (let [retried-results (retry-until-failure commands state-machine)]
                      (check-interleaved-postconditions retried-results commands state-machine))
                    (if (failing? results)
                      (assert false results)
                      true)))))
