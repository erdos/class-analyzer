(ns class-analyzer.core
  (:import [java.io DataInputStream])
  (:require [clojure.java.io :as io :refer [file]]
            [class-analyzer.signature :as signature]))


(set! *warn-on-reflection* true)


(defmacro ^:private  str! [x] `(doto ~x (assert ~(str "Not string: " (pr-str x)))))


;; https://medium.com/@davethomas_9528/writing-hello-world-in-java-byte-code-34f75428e0ad
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html


(def ^:private ^java.nio.charset.Charset UTF-8 (java.nio.charset.Charset/forName "UTF-8"))


(defn- stream->str [^DataInputStream is len]
  (let [ba (byte-array len)]
    (loop [read-len 0]
      (if (= read-len len)
        (new String ba UTF-8)
        (recur (+ read-len (.read is ba read-len (- len read-len))))))))


(defn- constant-pool-record [^DataInputStream ois]
  (case (.readUnsignedByte ois)
    7  [:class (.readUnsignedShort ois)] ;; tag + name idx
    9  [:fieldref [(.readUnsignedShort ois) (.readUnsignedShort ois)]] ;; tag + classidx + nameandtype idx
    10 [:methodref [(.readUnsignedShort ois) (.readUnsignedShort ois)]] ;; tag + class idx + nameandtype idx
    8  [:string (.readUnsignedShort ois)]

    3  [:integer (.readInt ois)] ;; int, short, char, byte, boolean
    4  [:float   (.readFloat ois)]

    ;; special - 8 byte
    5  [:long (.readLong ois)]
    6  [:double (.readDouble ois)]

    12 [:nameandtype [(.readUnsignedShort ois)
                      (.readUnsignedShort ois)]] ;; tag nameidx descriptoridx

    1  (let [length (.readUnsignedShort ois)]
         [:utf8 (stream->str ois length)])

    11 [:interfacemethodref [(.readUnsignedShort ois) (.readUnsignedShort ois)]] ;; tag + class idx + nameandtypeidx

    15 [:methodhandle [(.readUnsignedByte ois) (.readUnsignedShort ois)]]
    16 [:methodtype (.readUnsignedShort ois)]

    ;; bootstrap_method_attr_index + name_and_type_idx
    18 [:invokedynamic [(.readUnsignedShort ois) (.readUnsignedShort ois)]]

    19 [:module (.readUnsignedShort ois)]

    20 [:package (.readUnsignedShort ois)]
    ))


(defn constant-pool [^DataInputStream ois]
  (let [cp-size (.readUnsignedShort ois)]
    (loop [acc (sorted-map)
           i 1]
      (if (< i cp-size)
        (let [[disc content] (constant-pool-record ois)
              delta (if (#{:long :double} disc) 2 1)]
          (recur (assoc acc i {:discriminator disc, :data content}) (+ i delta)))
        acc))))


(defn- cp-enhance-1 [pool]
  (reduce-kv
   (fn [acc k v]
     (case (:discriminator v)
       (:class :string :module :package) (update-in acc [k :data] (comp :data acc))

       :nameandtype (update acc k merge {:name (-> v :data first acc :data str!)
                                         :type (-> v :data second acc :data str!)})
       acc))
   pool pool))


(defn- cp-enhance-2 [pool]
  (reduce-kv
   (fn [acc k v]
     (case (:discriminator v)
       (:fieldref :methodref :interfacemethodred)
       (update acc k merge {:class (-> v :data first acc :data str!)
                            :name  (-> v :data second acc :name str!)
                            :type (-> v :data second acc :type str!)})
       acc))
   pool pool))


(def read-attribute nil)
(defmulti read-attribute (fn [discrinimator input-stream name length constant-pool] name))


(defmethod read-attribute :default [_ ^DataInputStream dis _ cnt _]
  (dotimes [_ cnt] (.readByte dis))
  :not-parsed)


(defmethod read-attribute "SourceFile" [_ ^java.io.DataInputStream dis _ _ constant-pool]
  (-> dis .readUnsignedShort constant-pool :data))


(defmethod read-attribute "Signature" [d ^java.io.DataInputStream dis _ _ constant-pool]
  (let [source (-> dis .readUnsignedShort constant-pool :data)]
    (signature/with-str source
      (-> (case d
            :method (signature/method-type-signature)
            :field (signature/field-type-signature)
            :class (signature/class-signature))
          (or (throw (ex-info "Could not parse signature!" {:discriminator d, :signature source})))))))

(defn read-attributes [discriminator ^java.io.DataInputStream ois constant-pool]
  (doall
   (for [i (range (.readUnsignedShort ois))
         :let [attr-name (-> ois .readUnsignedShort constant-pool :data str!)
               attr-len (.readInt ois)
               attr     (read-attribute discriminator ois attr-name attr-len constant-pool)]]
     (if (map? attr)
       (merge {:name attr-name} attr)
       {:name  attr-name
        :value attr}))))


(defn parse-access-flags [n]
  {:public     (bit-test n 0)
   :private    (bit-test n 1)
   :protected  (bit-test n 2)
   :static     (bit-test n 3)
   :final      (bit-test n 4)
   :super      (bit-test n 5) ;; Not final, can be extended
   :volatile   (bit-test n 6)
   :transient  (bit-test n 7)
   :interface  (bit-test n 9)
   :abstract   (bit-test n 10)
   :synthetic  (bit-test n 12) ;; generated
   :annotation (bit-test n 13)
   :enum       (bit-test n 14)
   :module     (bit-test n 15)})


(defn- read-methods [^java.io.DataInputStream ois constant-pool]
  (doall
   (for [i (range (.readUnsignedShort ois))
         :let [access-flags (.readUnsignedShort ois)
               name-idx     (.readUnsignedShort ois)
               descr-idx    (.readUnsignedShort ois)
               attrs        (read-attributes :method ois constant-pool)]]
     {:access (parse-access-flags access-flags)
      :name   (-> name-idx constant-pool :data str!)
      :descr  (-> descr-idx constant-pool :data str! (signature/with-str (signature/method-type-signature)))
      :attrs  attrs})))

(defn read-fields [^java.io.DataInputStream ois constant-pool]
  (doall
   (for [i (range (.readUnsignedShort ois))
         :let [access-flags (-> ois .readUnsignedShort parse-access-flags)
               name         (-> ois .readUnsignedShort constant-pool :data str!)
               descr        (-> ois .readUnsignedShort constant-pool :data str!)
               attributes   (read-attributes :field ois constant-pool)]]
     {:name   name
      :access access-flags
      :descr  (signature/with-str descr (signature/field-type-signature))
      :attributes attributes})))

(defn- read-interfaces [^DataInputStream ois constant-pool]
  (doall
   (for [i (range (.readUnsignedShort ois))
         :let [class-idx (.readUnsignedShort ois)]]
     (-> class-idx constant-pool :data str!))))


(defn read-class [input-stream]
  (let [ois (new java.io.DataInputStream input-stream)]

    ;; magic number
    (assert (= 51966 (.readUnsignedShort ois))) ;; CA FE
    (assert (= 47806 (.readUnsignedShort ois))) ;; BA BE

    (let [[minor major] [(.readUnsignedShort ois) (.readUnsignedShort ois)]

          pool          (-> ois constant-pool cp-enhance-1 cp-enhance-2)
          access-flags  (.readUnsignedShort ois)
          class         (-> ois .readUnsignedShort pool :data str!)
          super-class   (-> ois .readUnsignedShort pool :data (or "java.lang.Object"))
          interfaces    (read-interfaces ois pool)
          fields        (read-fields ois pool)
          methods       (read-methods ois pool)
          attrs         (read-attributes :class ois pool)]
      {:version {:minor minor :major major}
       :class       class
       :super-class super-class
       :interfaces  interfaces
       :attributes  attrs
       :fields      fields
       :methods     methods
       :access      (parse-access-flags access-flags)
       :constants   pool})))
