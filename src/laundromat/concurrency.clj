(ns laundromat.concurrency
  (:require [clojure.core.memoize :as memo]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [laundromat.core :refer :all]
            [clojure.math.combinatorics :as combinatorics]))

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
    (map-indexed
      (fn [index _]
        {:sequential sequential
         :concurrent (exclude-nth index concurrent)})
      concurrent)))

(defn move-concurrent->sequential [generated-commands]
  (let [{:keys [sequential concurrent]} generated-commands]
    (map-indexed
      (fn [index _]
        {:sequential (concat sequential [(nth concurrent index)])
         :concurrent (exclude-nth index concurrent)})
      concurrent)))

(defn shrink-concurrent-commands [generated-commands]
  (lazy-cat
    (remove-sequential generated-commands)
    (remove-concurrent generated-commands)
    (move-concurrent->sequential generated-commands)))

(defn shrink-tree-concurrent-commands [generated-commands]
  [generated-commands
   (map shrink-tree-concurrent-commands (shrink-concurrent-commands generated-commands))])

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
  (prop/for-all [commands (gen-concurrenct-commands state-machine)]
                false
                )
  )
