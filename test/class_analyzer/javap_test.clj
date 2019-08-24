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

#_
(deftest all-clojure-classes
  (->>
   (sort (j/jar-classes example-jar))
   (reverse)
   ; (filter #(.startsWith (str %) "clojure.lang."))
   ;; (drop-while #(not (.startsWith (str %) "clojure.lang.Binding")))
   ;; (filter #(.endsWith (str %) "TransactionalHashMap"))
   (pmap (fn [class-name ]
           (println "Testing" class-name)
           (testing class-name
             (is (= (javap-output example-jar class-name)
                    (own-output example-jar (str (.replace (str class-name) "." "/") ".class")))))))
   doall))


;; (render (j/zip-open-file example-jar "clojure/lang/Compiler.class" c/read-class))

;; (clojure.pprint/pprint (j/zip-open-file example-jar "clojure/lang/TransactionalHashMap.class" c/read-class))
