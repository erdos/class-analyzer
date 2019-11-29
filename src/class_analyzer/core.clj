(ns class-analyzer.core
  (:import [java.io DataInputStream])
  (:require [clojure.java.io :as io :refer [file]]
            [class-analyzer.util :refer :all]
            [class-analyzer.signature :as signature]))


(set! *warn-on-reflection* true)

(defn- ->class-name [s]
  (assert (string? s))
  (symbol (.replace ^String s  "/" ".")))

;; https://medium.com/@davethomas_9528/writing-hello-world-in-java-byte-code-34f75428e0ad
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html


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
       :invokedynamic (update acc k assoc
                              :bootstrap-method-attr-idx (-> v :data first)
                              :method-name (-> v :data second acc :name)
                              :method-type (-> v :data second acc :type))

       (:fieldref :methodref :interfacemethodref)
       (update acc k assoc
               :class (-> v :data first acc :data str!)
               :name  (-> v :data second acc :name str!)
               :type (-> v :data second acc :type str!))
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


(defmethod read-attribute "Exceptions" [d ^java.io.DataInputStream dis _ _ constant-pool]
  (doall
   (for [i (range (.readUnsignedShort dis))]
     (-> dis .readUnsignedShort constant-pool :data ->class-name))))

(defn- read-verification-type-info [^DataInputStream d]
  (case (.readUnsignedByte d)
    0 {:item :top}
    1 {:item :integer}
    2 {:item :float}
    4 {:item :long}
    3 {:item :double}
    5 {:item :null}
    6 {:item :uninitialized-this}
    7 {:item       :object
       :object-idx (.readUnsignedShort d)}
    8 {:item       :uninitialized-variable
       :offset     (.readUnsignedShort d)}))

(defmethod read-attribute "StackMapTable" [_ ^DataInputStream dis _ _ constant-pool]
  (doall
    (for [i (range (.readUnsignedShort dis))
          :let [d (.readUnsignedByte dis)]]
      (cond (<= 0 d 63)   {:type :same-frame}
            (<= 64 d 127) {:type :same_locals_1_stack_item_frame
                           :stack (read-verification-type-info dis)}
            (= 247 d)     {:type :same_locals_1_stack_item_frame_extended
                           :offset-delta (.readUnsignedShort dis)
                           :stack (read-verification-type-info dis)}
            (<= 248 d 250) {:type :chop-frame
                            :offset-delta (.readUnsignedShort dis)}
            (= 251 d)      {:type :same-frame-extended
                            :offset-delta (.readUnsignedShort dis)}
            (<= 252 d 254) {:type :append-frame
                            :offset-delta (.readUnsignedShort dis)
                            :data (vec (repeatedly (- d 251) (partial read-verification-type-info dis)))}
            (= 255 d)      {:type :full-frame
                            :offset-delta (.readUnsignedShort dis)
                            :locals (vec (repeatedly (.readUnsignedShort dis)
                                                     (partial read-verification-type-info dis)))
                            :stack  (vec (repeatedly (.readUnsignedShort dis)
                                                     (partial read-verification-type-info dis)))}))))


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


;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1-200-E.1
(defn parse-class-flags [n]
  {:public     (bit-test n 0)
   :private    (bit-test n 1) ;; nested class
   :protected  (bit-test n 2) ;; nested class
   :static     (bit-test n 3) ;; nested class
   :final      (bit-test n 4)
   :super      (bit-test n 5)
   :interface  (bit-test n 9)
   :abstract   (bit-test n 10)
   :synthetic  (bit-test n 12)
   :annotation (bit-test n 13)
   :enum       (bit-test n 14)})


(defn parse-field-flags [n]
  {:public     (bit-test n 0)
   :private    (bit-test n 1)
   :protected  (bit-test n 2)
   :static     (bit-test n 3)
   :final      (bit-test n 4)
   :volatile   (bit-test n 6)
   :transient  (bit-test n 7)
   :synthetic  (bit-test n 12)
   :enum       (bit-test n 14)
   })


(defn parse-method-flags [n]
  {:public     (bit-test n 0)
   :private    (bit-test n 1)
   :protected  (bit-test n 2)
   :static     (bit-test n 3)
   :final      (bit-test n 4)
   :synchronized (bit-test n 5)
   :bridge     (bit-test n 6)
   :varargs    (bit-test n 7)
   :native     (bit-test n 8)
   :abstract   (bit-test n 10)
   :strict     (bit-test n 11)
   :synthetic  (bit-test n 12)})


(defn- read-methods [^java.io.DataInputStream ois constant-pool]
  (doall
   (for [i (range (.readUnsignedShort ois))
         :let [access-flags (.readUnsignedShort ois)
               name-idx     (.readUnsignedShort ois)
               descr-idx    (.readUnsignedShort ois)
               attrs        (read-attributes :method ois constant-pool)]]
     {:access (parse-method-flags access-flags)
      :name   (-> name-idx constant-pool :data str!)
      :descr  (-> descr-idx constant-pool :data str! (signature/with-str (signature/method-type-signature)))
      :attrs  attrs})))


(defn read-fields [^java.io.DataInputStream ois constant-pool]
  (doall
   (for [i (range (.readUnsignedShort ois))
         :let [access-flags (-> ois .readUnsignedShort parse-field-flags)
               name         (-> ois .readUnsignedShort constant-pool :data str!)
               descr        (-> ois .readUnsignedShort constant-pool :data str!)
               attributes   (read-attributes :field ois constant-pool)]]
     {:name   name
      :access access-flags
      :descr-raw descr
      :descr  (signature/with-str descr (signature/field-descriptor))
      :attributes attributes})))


(defn- read-interfaces [^DataInputStream ois constant-pool]
  (doall
   (for [_    (range (.readUnsignedShort ois))
         :let [class-idx (.readUnsignedShort ois)]]
     (-> class-idx constant-pool :data ->class-name))))


(defn read-class [input-stream]
  (let [ois (new java.io.DataInputStream input-stream)]

    ;; magic number
    (assert (= 0xCAFE (.readUnsignedShort ois)))
    (assert (= 0xBABE (.readUnsignedShort ois)))

    (let [[minor major] [(.readUnsignedShort ois) (.readUnsignedShort ois)]

          pool          (-> ois constant-pool cp-enhance-1 cp-enhance-2)
          access-flags  (.readUnsignedShort ois)
          class         (-> ois .readUnsignedShort pool :data ->class-name)
          super-class   (some-> ois .readUnsignedShort pool :data ->class-name)
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
       :access      (parse-class-flags access-flags)
       :constants   pool})))
