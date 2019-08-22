(ns class-analyzer.core-test
  (:import [java.util.jar JarEntry])
  (:require [clojure.test :refer [deftest testing is are]]
            [class-analyzer.core :refer :all]
            [class-analyzer.jar :refer :all]
            [clojure.java.io :refer [file]]))


(deftest test-parse-class-flags
  (are [kw nr] (kw (parse-class-flags nr))
    :public     0x0001
    :private    0x0002
    :protected  0x0004
    :static     0x0008
    :final      0x0010
    :super      0x0020
    :interface  0x0200
    :abstract   0x0400
    :synthetic  0x1000
    :annotation 0x2000
    :enum       0x4000))


(deftest test-parse-field-flags
  (are [kw nr] (kw (parse-field-flags nr))
    :public     0x0001
    :private    0x0002
    :protected  0x0004
    :static     0x0008
    :final      0x0010
    :volatile   0x0040
    :transient  0x0080
    :synthetic  0x1000
    :enum       0x4000))


(deftest test-parse-method-flags
  (are [kw nr] (kw (parse-method-flags nr))
    :public       0x0001
    :private      0x0002
    :protected    0x0004
    :static       0x0008
    :final        0x0010
    :synchronized 0x0020
    :bridge       0x0040
    :varargs      0x0080
    :native       0x0100
    :abstract     0x0400
    :strict       0x0800
    :synthetic    0x1000))
