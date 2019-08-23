(ns class-analyzer.javap-test
  (:require [class-analyzer.javap :refer :all]
            [class-analyzer.jar :as j]
            [class-analyzer.core :as c]
            [clojure.java.shell :refer [sh]]
            [clojure.test :refer [deftest testing is are]]))

(def home
  (doto (System/getenv "HOME")
    (assert "Missing $HOME env var!")))

(def example-jar (str home "/.m2/repository/org/clojure/clojure/1.10.1/clojure-1.10.1.jar"))

(defn- own-output [file class]
  (clojure.string/split-lines (with-out-str (render (j/zip-open-file file class c/read-class)))))

(defn- javap-output [file class]
  (clojure.string/split-lines (:out (sh "javap"  "-classpath" file class))))

(deftest parse-clojure-util-class
  (is (= (javap-output example-jar "clojure.lang.Util")
         (own-output example-jar "clojure/lang/Util.class"))))

(deftest parse-clojure-iobj-interface
  (is (= (javap-output example-jar "clojure.lang.IObj")
         (own-output example-jar "clojure/lang/IObj.class"))))

(deftest parse-clojure-aseq
  (is (= (javap-output example-jar "clojure.lang.ASeq")
         (own-output example-jar "clojure/lang/ASeq.class"))))

(deftest parse-clojure-aseq
  (is (= (javap-output example-jar "clojure.lang.IAtom2")
         (own-output example-jar "clojure/lang/IAtom2.class"))))

(deftest parse-clojure-rt
  (is (= (javap-output example-jar "clojure.lang.RT")
         (own-output example-jar "clojure/lang/RT.class"))))

;; good: Ratio, ProxyHandler, Range, PersistentTreeSet

#_
(deftest parse-clojure-range
  (is (= (javap-output example-jar "clojure.lang.PersistentTreeSet")
         (own-output example-jar "clojure/lang/PersistentTreeSet.class"))))

;; (render (j/zip-open-file example-jar "clojure/lang/RT.class" c/read-class))
