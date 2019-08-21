(ns class-analyzer.jar
  (:import [java.util.jar JarEntry])
  (:require [clojure.java.io :as io :refer [file]]))


(set! *warn-on-reflection* true)


(defn jar-entries [jar-path]
  (assert (.exists (file jar-path))
          (str "No such file: " (file jar-path)))
  (let [jar-file (new java.util.jar.JarFile (file jar-path) false)]
    (enumeration-seq (.entries jar-file))))


(defn jar-classes [jar-path]
  (for [^JarEntry entry (jar-entries jar-path)
        :let [entry-name (.getName entry)]
        :when (.endsWith entry-name ".class")]
    (-> entry-name
        (.substring 0 (- (count entry-name) 6))
        (.replace "/" ".")
        ; (.replace "$" ".")
        )))


(defn zip-open [input-file]
  (let [fis (new java.io.FileInputStream (file input-file))
        zis (new java.util.zip.ZipInputStream fis)]
    [zis
     (for [i (range)
           :let [entry (.getNextEntry zis)]
           :while (some? entry)
           :when (.endsWith (.getName entry) ".class")
           ]
       (.getName entry)
       )]))

(defn zip-open-file [input-file class-name reader-fn]
  (let [[zis fs] (zip-open input-file)]
    (with-open [zis zis]
      (first (for [f fs :when (= f class-name)] (reader-fn zis))))))
