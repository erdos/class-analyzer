(ns class-analyzer.signature-test
  (:require [class-analyzer.signature :refer :all]
            [clojure.test :refer [deftest testing is are]])
    (:import [java.io Reader BufferedReader StringReader PushbackReader]))


;; import private vars from tested namespace
(let [target (the-ns 'class-analyzer.signature)]
  (doseq [[k v] (ns-map target)
          :when (and (var? v) (= target (.ns v)))]
    (eval `(defn ~(symbol (str "-" k)) [~'& args#] (apply (deref ~v) args#)))))


(deftest test-identifier
  (testing "Positive"
    (is (= "asdf1" (with-str "asdf1;xx" (identifier)))))
  (testing "Not found"
    (is (= nil (with-str ";xx" (identifier))))
    (is (= nil (with-str " asdf" (identifier)))))
  (testing "Reset on error")

  (testing "Error on EOF"

    ))


(deftest test-expect
  (with-str "abc"
    (is (nil? (-expect \X)))
    (is (-expect \a))
    (is (nil? (-expect \B)))
    (is (-expect \b))
    (is (-expect \c))
    (is (nil? (-expect \d)))))


(deftest test-either
  (with-str "bc"
    (is (= \b (-either #(-expect \a) #(-expect \b))))
    (is (= nil (-either #(-expect \a))))
    (is (= \c (-either #(-expect \c))))
    (is (= nil (-either #(-expect \a))))))


(deftest test-separated-tokens-1
  (testing "Read 1 item"
    (with-str "a!"
      (is (= [\a]
             (-separated-tokens-1 \, #(-expect \a))))
      (is (= \! (char (.read *reader*))))))
  (testing "Read 4 items in a row"
    (with-str "a.b.c"
      (is (= ["a" "b" "c"] (-separated-tokens-1 \. identifier)))))
  (testing "Could not read so reader is reverted"
    (with-str "xyz"
      (is (= nil (-separated-tokens-1 \, #(-expect \a))))
      (is (= \x (char (.read *reader*)))))))


(deftest test-class-type-signature
  (testing "Simple class"
    (is (= {:package "java.lang"
            :class "Object"}
           (with-str "Ljava/lang/Object;"
             (-class-type-signature)))))

  (testing "Inner class"
    (with-str "Ljava/util/List.Entry;"
      (-class-type-signature)))
  (testing "Generic class"
    (with-str "Ljava/util/List<TT;>;"
      (-class-type-signature))))


(deftest test-type-signature
  (with-str "Ljava/lang/reflect/Constructor<*>;"
    (is (= {:package "java.lang.reflect", :class "Constructor", :generic [\*]}
           (-type-signature)))))


(deftest test-method-type-signature
  (with-str "(Ljava/lang/reflect/Constructor<*>;)Lclojure/asm/commons/Method;"
    (is (= {:type-params nil
            :args [{:package "java.lang.reflect", :class "Constructor", :generic [\*]}]
            :return {:package "clojure.asm.commons", :class "Method"}, :throws []}
           (method-type-signature)))))


(deftest test-class-signature
  ; (-class-signature (str-pbr "<T:Ljava/lang/Object;>Ljava/util/List<TT;>;"))
  (is (=
       {:formal-type-parameters [{:identifier "T"
                                  :classbound {:package "java.lang", :class "Object"}
                                  :interfacebound []}]
        :superclass {:package "java.lang", :class "Object"}
        :superinterface [{:package "java.lang"
                          :class "Iterable"
                          :generic [{:indicator nil, :field-type-signature "T"}]}]}
       (with-str "<T:Ljava/lang/Object;>Ljava/lang/Object;Ljava/lang/Iterable<TT;>;"
         (class-signature)))))
