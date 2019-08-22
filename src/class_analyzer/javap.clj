(ns class-analyzer.javap
  (:require [class-analyzer.core :as c]
            [class-analyzer.signature :as signature]))

(set! *warn-on-reflection* true)

(def ^:dynamic *verbose* false)
(def ^:dynamic *level* :public) ;; :public :protected :package :private

(defn- render-accessors [m]
  (cond-> []
    (:public m) (conj "public")
    (:static m) (conj "static")
    (:final m)  (conj "final")))

(defn- print-field [f]
  (apply print \space (render-accessors (:access f)))
  (println (str \space (-> f :descr signature/render-type) \space (:name f) ";")))

(defn- print-ctor [obj m]
  (apply print \space (render-accessors (:access m)))
  (print (str \space (:class obj)))
  (println (str "("
                (clojure.string/join ", " (map signature/render-type (:args (:descr m))))
                ");")))

(defn- print-static-init [obj m]
  (println "  static {};"))

(defn- print-method [obj m]
  (apply print \space (render-accessors (:access m)))
  (print (str \space (-> m :descr :return signature/render-type) \space (:name m)))
  (println (str "("
                (clojure.string/join ", " (map signature/render-type (:args (:descr m))))
                ");")))

(defn print-method* [obj m]
  (case (:name m)
    "<init>" (print-ctor obj m)
    "<clinit>" (print-static-init obj m)
    (print-method obj m)))


(defn render [obj]
  (run! print (render-accessors (:access obj)))

  (print " class" (:class obj))

  (when-let [sc (:super-class obj)]
    (print " extends" sc))

  (when-let [is (seq (:interfaces obj))]
    (print " implements" (clojure.string/join ", " is)))

  (println " {")

  (run! print-field (:fields obj))

  (run! (partial print-method* obj) (:methods obj))

  (println "}"))
