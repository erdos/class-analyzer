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


(deftest all-clojure-classes
  (doseq [class-name (sort (j/jar-classes example-jar))]
    (println "Testing" class-name)
    (testing class-name
      (is (= (javap-output example-jar class-name)
             (own-output example-jar (str (.replace (str class-name) "." "/") ".class")))))))


;; (render (j/zip-open-file example-jar "clojure/lang/Compiler.class" c/read-class))

;; (clojure.pprint/pprint (j/zip-open-file example-jar "clojure/lang/Compiler$C.class" c/read-class))

;; TODO: nem kezeli, ha az extends/implements resz is generikus!!

;; field tipus generikust sem kezelte!!


;; clojure.lang.ArrayIter$ArrayIter_long
;; clojure.lang.ArrayIter$ArrayIter_short
;; clojure.lang.Compiler$CaseExpr
;; clojure.lang.Compiler$C

;; clojure.lang.Ref
;; clojure.lang.ArrayIter$ArrayIter_float
