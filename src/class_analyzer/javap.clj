(ns class-analyzer.javap
  (:refer-clojure :exclude [print-method])
  (:require [class-analyzer.core :as c]
            [class-analyzer.signature :as signature]
            [class-analyzer.opcodes :refer [mnemonic->args]]))

(require 'class-analyzer.code)

(set! *warn-on-reflection* true)


(def ^:dynamic *verbose* false)
(def ^:dynamic *level* :public) ;; :public :protected :package :private
(def ^:dynamic *print-code* false) ;; show compiled code
(def ^:dynamic *print-signatures* false) ;; print internal type signatures

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
    (when *print-code* (println))
    ; (println) ;; only when code is printed!!
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

(defn- ^String str-unq [^String s]
  (assert (string? s))
  (if (.startsWith s "\"") (.substring s 1 (- (count s) 1)) s))

(defn class-name [^String s]
  (if (.startsWith s "[") (str "\"" s "\"") s))

(defn- str-right [n s]
  (let [s (str s)]
    (str (apply str (repeat (max 1 (- n (count s))) " ")) s)))

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
      (let [spaces (apply str (repeat (max 1 (- 14 (count (name (:mnemonic code))))) " "))
            rightpad  (fn [s n] (apply str s (repeat (- n (count (str s))) " ")))
            full? (not= (.replace (str (:class x)) "/" ".") (name current-class)) ;; fully qualified class?
            mname     (fn [s] (if (= "<init>" s) (pr-str s) s))]

        (if-let [second-arg (second (:args code))]
          (print (str spaces "#" (rightpad (str (first (:args code)) ",  " second-arg) 17)))
          (print (str spaces "#" (rightpad (first (:args code)) 19))))
        (case (:discriminator x)
          :string    (print (str "// String" (-> (:data x) (->> (str " ")) (.replace "\"" "\\\"") (.replace "\n" "\\n") (.replaceAll "\\s+$" ""))))
          (:long :float :double :boolean :short :char :int)
          (print "//" (name (:discriminator x))
                      (str (:data x) ({:long "l" :double "d" :float "f"} (:discriminator x))))

          :invokedynamic
          (print "//" "InvokeDynamic" (str "#" (:bootstrap-method-attr-idx x) ":" (:method-name x) ":" (:method-type x)))

          :interfacemethodref (print (str "// InterfaceMethod " (when full? (str (:class x) ".")) (mname (:name x)) ":" (:type x)))
          :methodref (print (str "// Method " (when full? (str (:class x) ".")) (mname (:name x)) ":" (:type x)))
          :fieldref  (print (str "// Field " (when full? (str (:class x) "."))  (:name x) ":" (:type x)))
          :class     (print (str "// class " (class-name (doto (:data x) (-> string? assert)))))))
      (let [spaces (apply str (repeat (- 14 (count (name (:mnemonic code)))) " "))
            rightpad  (fn [s n] (apply str s (repeat (- n (count (str s))) " ")))]
        (when   (= :tableswitch (:mnemonic code))
          (println "   { //" (:low code) "to" (+ (:high code))) ;; TODO: dynamic values
          (doseq [[k v] (:offsets code)]
            (printf "                     %d: %d\n" k (+ (:offset code) v))) ;; TODO: dynamic leftpad
          (println  "               default:" (+ (:offset code) (:default code)))
          (print "          }")
          )

          (when   (= :lookupswitch (:mnemonic code))
            (println "   { //" (count (:offsets code))) ;; TODO: dynamic values
            (doseq [{:keys [match offset]} (:offsets code)]
              (printf "                     %d: %d\n" match (+ (:offset code) offset))) ;; TODO: dynamic leftpad
            (println  "               default:" (+ (:offset code) (:default code)))
            (print "          }")
            )
        (when-let [a (first (:args code))]
          (cond

          (= [:branchoffset] (mnemonic->args (:mnemonic code)))
          (print (str spaces (+ (:offset code) a)))

          (= [:byte :byte] (mnemonic->args (:mnemonic code)))
          (print (str spaces (first (:args code)) ", " (second (:args code))))

          (= [:typecode] (mnemonic->args (:mnemonic code)))
          (print spaces  (case (int a)
                               4 'boolean
                               5 'char
                               6 'float
                               7 'double
                               8 'byte
                               9 'short
                               10 'int
                               11 'long))

          (#{[:byte] [:short]} (mnemonic->args (:mnemonic code)))
          (print (str spaces a))

          ))))
    (println))

    (when-let [table (seq (:exception-table attribute))]
      (println "    Exception table:")
      (println "       from    to  target type")
      (doseq [row table]
        (println (str (str-right 12 (:start-pc row)) (str-right 6 (:end-pc row)) (str-right 6 (:handler-pc row))) " "
           (if (= :any (:catch-type row))
             "any"
             (str "Class " (:data (:catch-type row))))))))

(defn method-visible? [m] (or (not (:private (:access m))) (= "<clinit>" (:name m))))

(defn print-method* [obj m]
  (case (:name m)
    "<init>"   (print-m-ctor obj m)
    "<clinit>" (print-static-init obj m)
    (print-method obj m))
  (when *print-code*
    (when-let [code (some #(when (= "Code" (:name %)) %) (:attrs m))]
      (print-code-attribute (:class obj) code))))

(defn- run-print! [f items]
  (when (seq items)
    (f (first items))
    (doseq [i (next items)]
      (println)
      (f i))))

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

  (->>
    (:methods obj)
    (filter method-visible?)
    ((if *print-code* run-print! run!) (partial print-method* obj)))
  (println "}")
  nil)
