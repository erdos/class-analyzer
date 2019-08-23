(ns class-analyzer.javap
  (:require [class-analyzer.core :as c]
            [class-analyzer.signature :as signature]))

(set! *warn-on-reflection* true)

(def ^:dynamic *verbose* false)
(def ^:dynamic *level* :public) ;; :public :protected :package :private

(defn- render-accessors [m]
  (->>
   (cond-> []
     (:public m)    (conj "public")
     (:protected m) (conj "protected")
     (:transient m) (conj "transient")
     (:static m)    (conj "static")
     (:volatile m)  (conj "volatile")
     (and (:abstract m) (not (:interface m))) (conj "abstract")
     (:final m)     (conj "final"))
   (clojure.string/join " ")
   (not-empty)))

(defn- print-field [f]
  (when (not (:private (:access f)))
    (print \space (render-accessors (:access f)))
    (println (str \space (-> f :descr signature/render-type) \space (:name f) ";"))))

(defn- render-generic [x]
  (cond
    (= \* x) "?"
    (string? x) x
    (:field-type-signature x) (recur (:field-type-signature x)) ;; ?
    (:package x)
    (str (:package x) "." (:class x)
         (some-> x :generic
                 (->> (map render-generic)
                      (clojure.string/join ", ")
                      (str "<"))
                 (str ">")))
    (:array x) (str (render-generic (:array x)) "[]")
    (keyword? x) (name x)
    :else (assert false (str "Unexpected!!" (pr-str x)))))

(defn print-method-args [obj m]
  (-> (if-let [[s] (seq (filter (comp #{"Signature"} :name) (:attrs m)))]
        (map render-generic (:args s))
        (map signature/render-type (:args (:descr m))))
      (->> (clojure.string/join ", "))
      ;; if it is a varargs method: replace last two characters with '...'
      (cond-> (:varargs (:access m))
        (as-> * (str (subs * 0 (- (count *) 2)) "...")))
      (as-> * (str "(" * ")"))
      (print)))

(defn print-throws [obj m]
  (when-let [e (some #(when (= "Exceptions" (:name %)) (:value %)) (:attrs m))]
    (print " throws" (clojure.string/join ", " e))))

(defn- print-method-generics [obj m]
  (when-let [[s] (seq (filter (comp #{"Signature"} :name) (:attrs m)))]
    (when-let [tp (:type-params s)]
      (print " <")
      (->>
       (for [t tp]
         (:identifier t)
         )
       (clojure.string/join ", ")
       (print))
      (print ">"))))

(defn- print-ctor [obj m]
  (if-let [as (render-accessors (:access m))]
    (print \space as)
    (print \space))
  (print-method-generics obj m)
  (print (str \space (:class obj)))
  (print-method-args obj m)
  (print-throws obj m)
  (println ";"))

(defn- print-static-init [obj m]
  (println "  static {};"))


;; TODO: generic info!!
;; TODO: throws!
(defn- print-method [obj m]
  (print \space (render-accessors (:access m)))

  (print-method-generics obj m)
  (print \space)
  (if-let [return (some #(when (= "Signature" (:name %)) (:return %)) (:attrs m))]
    (print (render-generic return))
    (print (-> m :descr :return signature/render-type)))
  (print \space)
  (print (:name m))
  (print-method-args obj m)
  (print-throws obj m)
  (println ";"))

(defn print-method* [obj m]
  (when (or (not (:private (:access m)))
            (= "<clinit>" (:name m)))
    (case (:name m)
      "<init>"   (print-ctor obj m)
      "<clinit>" (print-static-init obj m)
      (print-method obj m))))


(defn render [obj]
  (when-let [sf (some #(when (= "SourceFile" (:name %)) (:value %)) (:attributes obj))]
    (println (str "Compiled from " \" sf \")))

  (print (render-accessors (:access obj)))

  (let [interface? (:interface (:access obj))]

    (print (if interface? " interface" " class") (:class obj))

    (when-let [sc (:super-class obj)]
      (if-not (= 'java.lang.Object sc)
        (print " extends" sc)))

    (when-let [is (seq (:interfaces obj))]
      (print (if interface? " extends" " implements")
             (clojure.string/join "," is))))

  (println " {")

  (run! print-field (:fields obj))

  (run! (partial print-method* obj) (:methods obj))

  (println "}")
  #_
  (clojure.pprint/pprint obj))
