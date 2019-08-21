(ns class-analyzer.javap
  (:require [class-analyzer.core :as c]
            [class-analyzer.jar :as j]
            [class-analyzer.signature :as signature]))

(set! *warn-on-reflection* true)

(def ^:dynamic *verbose* false)
(def ^:dynamic *level* :public) ;; :public :protected :package :private


(defn main [& args]

  )


(def example-jar "/home/erdos/.m2/repository/org/clojure/clojure/1.10.1/clojure-1.10.1.jar")

(defn- render-accessors [m]
  (cond-> []
    (:public m) (conj "public")
    (:static m) (conj "static")
    (:final m)  (conj "final")))

(defn- print-field [f]
  (apply print \space (render-accessors (:access f)))
  (println (str \space (-> f :descr signature/render-type) \space (:name f) ";")))

(defn- print-method [obj m]
  (apply print \space (render-accessors (:access m)))
  (if (= "<init>" (:name m))
    (print (str \space (:class obj)))
    (print (str \space (-> m :descr :return signature/render-type) \space (:name m))))
  (println (str "("
                (clojure.string/join ", " (map signature/render-type (:args (:descr m))))
                ");")))

(let [obj (j/zip-open-file example-jar "clojure/lang/Ratio.class" c/read-class)]
  (run! print (render-accessors (:access obj)))

  (print " class" (:class obj))
  (when-let [sc (:super-class obj)]
    (print " extends" sc))
  (when-let [is (:interfaces obj)]
    (print " implements" (clojure.string/join ", " is)))

  (println " {")

  (run! print-field (:fields obj))

  (run! (partial print-method obj) (:methods obj))

  (println "}"))
