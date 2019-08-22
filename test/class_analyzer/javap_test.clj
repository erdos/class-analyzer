(ns class-analyzer.javap-test
  (:require [class-analyzer.javap :refer :all]
            [class-analyzer.jar :as j]
            [class-analyzer.core :as c]
            [clojure.test :refer [deftest testing is are]]))

(def example-jar "/home/erdos/.m2/repository/org/clojure/clojure/1.10.1/clojure-1.10.1.jar")

(deftest t1
  (render (j/zip-open-file example-jar "clojure/lang/Util.class" c/read-class)))
