(defproject io.github.erdos/class-analyzer "0.1.0-SNAPSHOT"
  :description "JVM class file parser and analyzer"
  :url "https://github.com/erdos/dig-class"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :test-selectors {:javap :javap
                   :default (complement :javap)}
  :main class-analyzer.main
  :aot :all
  :dependencies [[org.clojure/clojure "1.10.1"]])
