(defproject stupid "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.7"]
                 [me.raynes/hickory "0.4.2"]
                 [me.raynes/laser "1.1.0" :exclusions [me.raynes/hickory]]]
  :main stupid.core
  :aot :all)
