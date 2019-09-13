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
  (clojure.string/split-lines (with-out-str (render (j/zip-open-file file class c/read-class)))))

(defn- javap-output [file class]
  (clojure.string/split-lines (:out (sh "javap" "-classpath" file "-c" class))))

(defn test-javap-output-matches [class-name]
  (println "Testing" class-name)
  (testing class-name
    (is (= (javap-output example-jar class-name)
           (own-output example-jar (str (.replace (str class-name) "." "/") ".class"))))))

#_
(deftest all-clojure-classes
  (->> (sort (j/jar-classes example-jar))
       (reverse)
       (pmap test-javap-output-matches)
       (doall)))

(deftest t1
  (test-javap-output-matches "clojure.asm.TypePath"))
