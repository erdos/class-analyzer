(ns ^:javap class-analyzer.javap-test
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
  (binding [*print-code* true]
    (clojure.string/split-lines
     (with-out-str
       (render
        (j/zip-open-file file class c/read-class))))))

(defn- javap-output [file class]
  (clojure.string/split-lines (:out (sh "javap" "-classpath" file "-c" class))))

;; we use this to compare lines without whitespce changes
(defn- is-seq-eq [expected actual]
  (is (= (count expected) (count actual)) "Lengths do not match!")
  (doseq [[e a] (map vector expected actual)
          :when (not= (.replaceAll (str e) " " "") (.replaceAll (str a) " " ""))]
     (is (= e a))))

(defn test-javap-output-matches [class-name]
  (println "Testing" class-name)
  (testing class-name
    (is-seq-eq (javap-output example-jar class-name)
               (own-output example-jar (str (.replace (str class-name) "." "/") ".class")))))

(deftest all-clojure-classes
  (->> (sort (j/jar-classes example-jar))
       (reverse)
       (map test-javap-output-matches)
       (doall)))

(deftest t1 (test-javap-output-matches "clojure.zip__init"))

#_
(deftest t1-interface (test-javap-output-matches "clojure.lang.Associative"))
