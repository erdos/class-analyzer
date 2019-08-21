(ns class-analyzer.core-test
  (:import [java.util.jar JarEntry])
  (:require [clojure.test :refer [deftest testing is are]]
            [class-analyzer.core :refer :all]
            [class-analyzer.jar :refer :all]
            [clojure.java.io :refer [file]]))


(deftest test-parse-access-flags
  (are [kw nr] (kw (parse-access-flags nr))
    :public    0x0001
    :protected 0x0004
    :static    0x0008
    :final     0x0010
    :super     0x0020
    :volatile  0x0040
    :transient 0x0080
    :interface 0x0200
    :abstract  0x0400
    :synthetic 0x1000
    :annotation 0x2000
    :enum      0x4000))
