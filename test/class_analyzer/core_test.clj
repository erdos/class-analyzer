(ns class-analyzer.core-test
  (:import [java.util.jar JarEntry])
  (:require [class-analyzer.core :refer :all]
            [class-analyzer.jar :refer :all]
            [clojure.java.io :refer [file]]))

                                        ; (def example-jar "/home/erdos/.m2/repository/commons-io/commons-io/2.6/commons-io-2.6.jar")
(def example-jar "/home/erdos/.m2/repository/org/clojure/clojure/1.10.1/clojure-1.10.1.jar")

#_
(with-open [fis (new java.io.FileInputStream (file example-jar))
            zis (new java.util.zip.ZipInputStream fis)]
  (time
   (doseq [i (range 10000)
           :let [entry (.getNextEntry zis)]
           :while (some? entry)
           :when (.endsWith (.getName entry) ".class")
           ; :when (.contains (.getName entry) "")
           ]
     (println "Entry: " entry)
     (-> (read-class zis)
         ; clojure.pprint/pprint
         )
     #_(throw (ex-info "a " {})))))
#_
(jar-classes example-jar)

#_
(def entry-0 (first (filter #(.contains (.getName ^JarEntry %) "ByteOrderParser") (jar-entries example-jar))))

#_
(defn- true-keys [m] (set (filter m (keys m))))
#_
;; super public
(assert (= #{:super :public} (true-keys (parse-access-flags 0x0021))))
