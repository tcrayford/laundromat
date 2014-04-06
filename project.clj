(defproject laundromat "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/test.check  "0.5.7"]
                 [org.clojure/math.combinatorics  "0.0.7"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [org.clojure/core.memoize "0.5.6"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/core.async "0.1.278.0-76b25b-alpha"]]}})
