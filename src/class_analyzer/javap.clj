(ns class-analyzer.javap
  (:require [class-analyzer.core :as c]
            [class-analyzer.signature :as signature]
            [class-analyzer.opcodes :refer [mnemonic->args]]))

(require 'class-analyzer.code)

(set! *warn-on-reflection* true)


(def ^:dynamic *verbose* false)
(def ^:dynamic *level* :public) ;; :public :protected :package :private


(defn- render-accessors [m]
  (->>
   (cond-> []
     (:public m)    (conj "public")
     (:protected m) (conj "protected")
     (:private m)    (conj "private")

     (:static m)    (conj "static")
     (and (:abstract m) (not (:interface m))) (conj "abstract")

     (:final m)     (conj "final") ;; final < transient,synchronized
     (:synchronized m) (conj "synchronized")

     (:transient m) (conj "transient")
     (:volatile m)  (conj "volatile")

     (:native m)     (conj "native")

     (:strict m)     (conj "strictfp"))
   (clojure.string/join " ")
   (not-empty)))


(defn- render-generic [x]
  (cond
    (= \* x) "?"
    (string? x) x
    (string? (:value x)) (:value x)
    (:field-type-signature x)
    (case (:indicator x)
      :extends (str "? extends " (render-generic (:field-type-signature x)))
      :super   (str "? super " (render-generic (:field-type-signature x)))
      (recur (:field-type-signature x)))
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


(defn- print-field [f]
  (when (not (:private (:access f)))
    (if-let [a (render-accessors (:access f))]
      (print (str \space \space a))
      (print \space))
    (print \space)

    (if-let [return (some #(when (= "Signature" (:name %)) %) (:attributes f))]
      (print (render-generic return))
      (print (-> f :descr signature/render-type)))

    (print \space)
    (print (:name f))
    (println ";")

    (println) ;; only when code is printed!!
    ))


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

(defn- print-m-ctor [obj m]
  (if-let [as (render-accessors (:access m))]
    (print \space as)
    (print \space))
  (print-method-generics obj m)
  (print (str \space (:class obj)))
  (print-method-args obj m)
  (print-throws obj m)
  (println ";"))


(defn- print-static-init [obj m]
  (if (:public (:access m))
    (println "  public static {};")
    (println "  static {};")))


(defn- print-method [obj m]
  (print \space)
  (when-let [rea (render-accessors (:access m))]
    (print (str \space rea)))

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

(defn- print-code-attribute [current-class attribute]
  (assert (symbol? current-class))
  (assert (= "Code" (:name attribute)))
  (println "    Code:")
  (doseq [code (:code attribute)]
    (printf "    %4d: %s"
            (:offset code)
            (name (:mnemonic code)))
    (if-let [x (first (:vals code))]
      ;; itt dinamikusan szamoljuk a szokozoket.
      (let [spaces (apply str (repeat (- 14 (count (name (:mnemonic code)))) " "))
            rightpad  (fn [s n] (apply str s (repeat (- n (count (str s))) " ")))
            full? (not= (.replace (str (:class x)) "/" ".") (name current-class)) ;; fully qualified class?
            mname     (fn [s] (if (= "<init>" s) (pr-str s) s))]
        (print (str spaces "#" (rightpad (first (:args code)) 19)))
        (case (:discriminator x)
          :methodref (print (str "// Method " (when full? (str (:class x) ".")) (mname (:name x)) ":" (:type x)))
          :fieldref  (print (str "// Field " (when full? (str (:class x) "."))  (:name x) ":" (:type x)))
          :class     (print (str "// class " (doto (:data x) (-> string? assert))  ))))
      (let [spaces (apply str (repeat (- 14 (count (name (:mnemonic code)))) " "))
            rightpad  (fn [s n] (apply str s (repeat (- n (count (str s))) " ")))]
        (when-let [a (first (:args code))]
          (cond

          (= [:branchoffset] (mnemonic->args (:mnemonic code)))
          (print (str spaces (+ (:offset code) a)))

          (= [:byte :byte] (mnemonic->args (:mnemonic code)))
          (print (str spaces (first (:args code)) ", " (second (:args code))))

          (= [:byte] (mnemonic->args (:mnemonic code)))
          (print (str spaces a))

          ))))
    #_(when-not (first (:vals code))
      (print (pr-str code)))
    (println))
  (println))
#_
(print-code-attribute
 {:name "Code",
  :max-stack 2,
  :max-locals 2,
  :code
  '({:args [],
    :vals [],
    :op-code 42,
    :mnemonic :aload_0,
    :offset 0,
    :nr 0}
   {:args [1],
    :vals

    [{:discriminator :methodref,
      :data [4 22],
      :class "java/lang/Object",
      :name "<init>",
      :type "()V"}],
    :op-code 183,
    :mnemonic :invokespecial,
    :offset 1,
    :nr 1}
   {:args [],
    :vals [],
    :op-code 42,
    :mnemonic :aload_0,
    :offset 4,
    :nr 2}
   {:args [],
    :vals [],
    :op-code 43,
    :mnemonic :aload_1,
    :offset 5,
    :nr 3}
   {:args [2],
    :vals
    [{:discriminator :fieldref,
      :data [3 23],
      :class "clojure/lang/Volatile",
      :name "val",
      :type "Ljava/lang/Object;"}],
    :op-code 181,
    :mnemonic :putfield,
    :offset 6,
    :nr 4}
   {:args [],
    :vals [],
    :op-code 177,
    :mnemonic :return,
    :offset 9,
    :nr 5}),
  :exception-table (),
  :attrs
  ({:name "LineNumberTable",
    :value
    ({:start-pc 0, :line-number 17}
     {:start-pc 4, :line-number 18}
     {:start-pc 9, :line-number
      19})}
   {:name "LocalVariableTable",
    :value
    ({:start-pc 0,
      :length 10,
      :name-idx "this",
      :descr-idx "Lclojure/lang/Volatile;",
      :index 0}
     {:start-pc 0,
      :length 10,
      :name-idx "val",
      :descr-idx "Ljava/lang/Object;",
      :index 1})})})



(defn print-method* [obj m]
  (when (or (not (:private (:access m)))
            (= "<clinit>" (:name m)))
    (case (:name m)
      "<init>"   (print-m-ctor obj m)
      "<clinit>" (print-static-init obj m)
      (print-method obj m))
    (when-let [code (some #(when (= "Code" (:name %)) %) (:attrs m))]
      (print-code-attribute (:class obj) code))))


(defn render [obj]
  (when-let [sf (some #(when (= "SourceFile" (:name %)) (:value %)) (:attributes obj))]
    (println (str "Compiled from " \" sf \")))

  (when-let [a (render-accessors (:access obj))]
    (print (str a \space)))

  (let [interface? (:interface (:access obj))]
    (print (if interface? "interface " "class "))

    (print (:class obj))

    (when-let [tps (some #(when (= "Signature" (:name %))
                            (:formal-type-parameters %)) (:attributes obj))]
      (print "<")
      (print (clojure.string/join ", " (map signature/render-formal-type-parameter tps)))
      (print ">"))

    (if-let [superclass (some #(when (= "Signature" (:name %)) (:superclass %)) (:attributes obj))]
      (when-not (= {:package "java.lang" :class "Object"} superclass)
        (print " extends" (render-generic superclass)))
      (when-let [sc (:super-class obj)]
        (if-not (= 'java.lang.Object sc)
          (print " extends" sc))))


    ;; ha implements van, akkor szorosan egymas mogott vannak
    (if-let [interfaces (some #(when (= "Signature" (:name %)) (seq (:superinterface %))) (:attributes obj))]
      (print (if interface? " extends" " implements")
             (clojure.string/join ", " (map render-generic interfaces)))
      (when-let [is (seq (:interfaces obj))]
        (print (if interface? " extends" " implements")
               (clojure.string/join "," is)))))

  (println " {")

  (run! print-field (:fields obj))

  (run! (partial print-method* obj) (:methods obj))

  (println "}")
  nil)
