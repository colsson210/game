(defproject game "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "2.7.1"]]
  :main ^:skip-aot game.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
  
  