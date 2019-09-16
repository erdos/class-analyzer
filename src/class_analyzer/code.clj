(ns class-analyzer.code
  (:import [java.io ByteArrayInputStream InputStream DataInputStream])
  (:require [class-analyzer.opcodes :refer :all]
            [class-analyzer.util :refer :all]
            [class-analyzer.core :as core]))

(set! *warn-on-reflection* true)

(def ^:private ^:dynamic ^java.io.DataInputStream *input-stream*)
(def ^:private ^:dynamic *byte-offset*)
(def ^:private ^:dynamic *constant-pool*)

(defn- read-byte []
  (swap! *byte-offset* inc)
  (.readByte *input-stream*))

(defn- read-unsigned-byte []
  (swap! *byte-offset* inc)
  (.readUnsignedByte *input-stream*))

(defn- read-short []
  (swap! *byte-offset* + 2)
  (.readShort *input-stream*))

(defn- read-unsigned-short []
  (swap! *byte-offset* + 2)
  (.readUnsignedShort *input-stream*))

(defn- read-int []
  (swap! *byte-offset* + 4)
  (.readInt *input-stream*))

#_
(defn- read-unsigned-int []
  (swap! *byte-offset* + 4)
  (.readUnsignedInt *input-stream*))

(defmulti ^:pivate read-op (fn [instruction-map] (:mnemonic instruction-map)))


(defn read-arg [a]
  (case a
    :cpidx2 (read-unsigned-short) ;; index in the constant pool
    :cpidx1 (read-unsigned-byte)  ;; index in the constant pool
    :branchoffset (read-short) ;; branch offset 2 bytes TODO: signed yes??
    :branchoffset4 (read-int) ;; 4 byte branch offset - maybe unsignded?
    :zerobyte (doto (read-byte) (-> zero? (assert "Expected zero byte!")))
    :byte (read-byte)
    :int (read-int)
    :short (read-short)))

(defmethod read-op :default [{:keys [mnemonic args]}]
  (assert (vector? args) (str "Not vector:  " (pr-str args)))
  (let [read (mapv read-arg args)]
    {:args read
     :vals (mapv (fn [code value]
                   (when (#{:cpidx1 :cpidx2} code)
                     (-> value *constant-pool*)))
                 args read)}))

(defmethod read-op :tableswitch [_]
  (dotimes [_ (- 4 (mod @*byte-offset* 4))] (read-byte))
  (let [low     (read-int)
        high    (read-int)
        default (read-int)
        offsets (doall (for [i (range (-> high (- low) (+ 1)))] (read-int)))]
    {:low     low
     :high    high
     :default default
     :offsets offsets}))

(defmethod read-op :lookupswitch [_]
  (let [n (- 4 (mod @*byte-offset* 4))]
    ; (assert false (str :n n))
    (dotimes [_ n] (read-byte))) ;; 0-3 byte offset
  (let [default     (read-int)
        npairs      (read-int)
        ; _ (assert false (str :npairs npairs :default default))
        match+offset (doall (for [i (range npairs)]
                              {:match (read-int)
                               :offset (read-int)}))]
    ;(assert false (str (pr-str match+offset)))
    {:default default
     :offsets match+offset}))

(defmethod read-op :wide [_]
  (assert false "Not impled! w"))

(defn- read-code [^InputStream istream]
  (binding [*byte-offset* (atom 0)
            *input-stream* (new java.io.DataInputStream istream)]
    (let [code-length      (.readInt *input-stream*)]
      (doall
       (for [i (range)
             :let [offset @*byte-offset*]
             :while (< offset code-length)
             :let [opcode (read-unsigned-byte)
                   instruction-map (get instructions opcode)]]
         (try
           (assoc (read-op instruction-map)
                  :op-code opcode
                  :mnemonic (:mnemonic instruction-map)
                  :offset offset
                  :nr     i)
           (catch Exception e
             (throw (ex-info "No code item" {:opcode opcode} e)))))

       ))))

(->
 [
  0x00 0x00 0x00 0x04 ;; int 1

  0x00 ;; NOP
  0x03 ;; iconst_0
  0x10 ;; bipush
  0x11 ;; constant arg
  ]
 (byte-array)
 (ByteArrayInputStream.)
 (DataInputStream.)
 (read-code)

 )

(defn read-exception-table []
  (doall
   (for [i (range (read-short))]
     {:start-pc   (read-short)
      :end-pc     (read-short)
      :handler-pc (read-short)
      :catch-type (read-short) ;; class info in constant pool
      })))


(defn read-attributes []
  ;; TODO: discriminator is either method or class?
  (core/read-attributes :method *input-stream* *constant-pool*))

(defn read-code-attribute []
  (binding [*byte-offset* (atom 0)]
    {:max-stack       (read-short)
     :max-locals      (read-short)
     :code            (read-code *input-stream*)
     :exception-table (read-exception-table)
     :attrs           (read-attributes)}))

(defmethod core/read-attribute "Code" [_ ^DataInputStream dis _ _ constant-pool]
  (binding [*constant-pool* constant-pool
            *input-stream* dis]
    (read-code-attribute)))

(defmethod core/read-attribute "LineNumberTable" [_ ^DataInputStream dis _ _ constant-pool]
  (doall
   (for [i (range (.readUnsignedShort dis))]
     {:start-pc    (.readUnsignedShort dis)
      :line-number (.readUnsignedShort dis)})))

(defmethod core/read-attribute "LocalVariableTable" [_ ^DataInputStream dis _ _ constant-pool]
  (doall
   (for [i (range (.readUnsignedShort dis))]
     {:start-pc    (.readUnsignedShort dis)
      :length      (.readUnsignedShort dis)
      :name-idx    (-> dis .readUnsignedShort constant-pool :data str!)
      :descr-idx   (-> dis .readUnsignedShort constant-pool :data str!)
      :index       (.readUnsignedShort dis)})))
