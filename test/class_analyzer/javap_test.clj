(ns ^:javap class-analyzer.javap-test
  (:require [class-analyzer.javap :refer :all]
            [class-analyzer.jar :as j]
            [class-analyzer.core :as c]
            [clojure.java.shell :refer [sh]]
            [clojure.test :refer [deftest testing is are]]))

(def ^:private home
  (doto (System/getenv "HOME")
    (assert "Missing $HOME env var!")))

(def ^:dynamic *jarfile*)

(defn- own-output [class]
  (binding [*print-code* true]
    (clojure.string/split-lines
     (with-out-str
       (render
        (j/zip-open-file (str home *jarfile*) class c/read-class))))))

(defn- javap-output [class]
  (clojure.string/split-lines (:out (sh "javap" "-classpath" (str home *jarfile*) "-c" class))))

;; we use this to compare lines without whitespce changes
(defn- is-seq-eq [expected actual]
  (is (= (count expected) (count actual)) "Lengths do not match!")
  (doseq [[e a] (drop-while (partial apply =) (map vector expected actual))
          :when (not= e a)]
     (is (= e a))))

(defn test-javap-output-matches [class-name]
  (println "Testing" class-name)
  (testing class-name
    (do (is-seq-eq (javap-output class-name)
                   (own-output (str (.replace (str class-name) "." "/") ".class"))))))

(defn all-jar-classes [] (j/jar-classes (str home *jarfile*)))

#_
(deftest all-clojure-classes
  (->> (all-jar-classes)
       (pmap test-javap-output-matches)
       (doall)
       (binding [*jarfile* "/.m2/repository/org/clojure/clojure/1.10.1/clojure-1.10.1.jar"])))

(deftest all-clojure-classes2
 (->> (all-jar-classes)
      (pmap test-javap-output-matches)
      (doall)
      (binding [*jarfile* "/.m2/repository/commons-lang/commons-lang/2.6/commons-lang-2.6.jar"])))

;  (deftest t1 (test-javap-output-matches "clojure.core__init"))
