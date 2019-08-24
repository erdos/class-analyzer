(ns class-analyzer.util-test
  (:import [java.io ByteArrayInputStream])
  (:require [class-analyzer.util :refer :all]
            [clojure.test :refer [deftest testing is are]]))

(deftest test-stream->str
  ;; TODO: test what happens when we want to read too many characters!
  (testing "Read nothing"
    (is (= "" (stream->str (ByteArrayInputStream. (.getBytes "asdf")) 0))))
  (testing "Read characters"
    (is (= "abc" (stream->str (ByteArrayInputStream. (.getBytes "abc")) 3)))
    (is (= "abc" (stream->str (ByteArrayInputStream. (.getBytes "abcd")) 3)))))
