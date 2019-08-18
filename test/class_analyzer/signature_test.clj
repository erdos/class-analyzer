(ns class-analyzer.signature-test
  (:require [class-analyzer.signature :refer :all]
            [clojure.test :refer [deftest testing is are]])
    (:import [java.io Reader BufferedReader StringReader PushbackReader]))

(let [target (the-ns 'class-analyzer.signature)]
  (doseq [[k v] (ns-map target)
          :when (and (var? v) (= target (.ns v)))]
    (eval `(defn ~(symbol (str "-" k)) [~'& args#] (apply (deref ~v) args#)))))

(defn- str-pbr [s] (PushbackReader. (StringReader. (str s))))





(deftest test-identifier
  (testing "Positive"
    (is (= "asdf1" (identifier (str-pbr "asdf1;xx")))))
  (testing "Not found"
    (is (= nil (identifier (str-pbr ";xx"))))
    (is (= nil (identifier (str-pbr " asdf")))))
  (testing "Reset on error")

  (testing "Error on EOF"

    ))

(deftest test-expect
  (let [r (str-pbr "abc")]
    (is (nil? (-expect r \X)))
    (is (-expect r \a))
    (is (nil? (-expect r \B)))
    (is (-expect r \b))
    (is (-expect r \c))
    (is (nil? (-expect r \d)))))

(deftest test-either
  (let [r (str-pbr "bc")]
    (is (= \b (-either r #(-expect % \a) #(-expect % \b))))
    (is (= nil (-either r #(-expect % \a))))
    (is (= \c (-either r #(-expect % \c))))
    (is (= nil (-either r #(-expect % \a))))))

(deftest test-separated-tokens-1
  (testing "Read 1 item"
    (let [r (str-pbr "a!")]
      (is (= [\a]
             (-separated-tokens-1 r \, #(-expect % \a))))
      (is (= \! (char (.read r))))))
  (testing "Read 4 items in a row"
    (is (= ["a" "b" "c"] (-separated-tokens-1 (str-pbr "a.b.c") \. identifier))))
  (testing "Could not read so reader is reverted"
    (let [r (str-pbr "xyz")]
      (is (= nil (-separated-tokens-1 r \, #(-expect % \a))))
      (is (= \x (char (.read r)))))))

(deftest test-class-type-signature
  (testing "Simple class"
    (is (= {:package "java.lang"
            :class "Object"}
           (-class-type-signature (str-pbr "Ljava/lang/Object;")))))

  (testing "Inner class"
    (-class-type-signature (str-pbr "Ljava/util/List.Entry;")))
  (testing "Generic class"
    (-class-type-signature (str-pbr "Ljava/util/List<TT;>;"))))


; (-class-signature (str-pbr "<T:Ljava/lang/Object;>Ljava/util/List<TT;>;"))
