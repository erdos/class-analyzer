(ns class-analyzer.code
  (:import [java.io ByteArrayInputStream InputStream DataInputStream])
  (:require [class-analyzer.opcodes :refer :all]
            [class-analyzer.util :refer :all]))

(def ^:private ^:dynamic ^java.io.DataInputStream *input-stream*)
(def ^:private ^:dynamic *byte-offset*)

(defn- read-byte []
  (println "Readin byte")
  (.readByte *input-stream*))

(defn- read-short []
  (println "Reading short")
  (.readShort *input-stream*))

(defn- read-int []
  (println "Reading int")
  (.readInt *input-stream*))

(defmulti ^:pivate read-op (fn [instruction-map] (:mnemonic instruction-map)))



(defn read-arg [a]
  (case a
    :cpidx2 (read-short) ;; index in the constant pool
    :cpidx1 (read-byte)  ;; index in the constant pool
    :branchoffset (read-short) ;; branch offset 2 bytes
    :branchoffset4 (read-int) ;; 4 byte branch offset
    :zerobyte (doto (read-byte) (-> zero (assert "Expected zero byte!")))
    :byte (read-byte)
    :int (read-int)
    :short (read-short)))

(defmethod read-op :default [{:keys [mnemonic args]}]
  (assert (vector? args))
  {:args (mapv read-arg args)})

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

(defn- read-code [^InputStream input-stream]
  (let [[counter istream] (wrap-counted input-stream)]
    (binding [*byte-offset* counter
              *input-stream* (new java.io.DataInputStream istream)]
      (let [code-length      (.readInt input-stream)]
        (println "Code length is " code-length)
        (reset! *byte-offset* 0)
        (doall
         (for [i (range)
               :let [offset @*byte-offset*
                     _ (println :? offset)]
               :while (< offset code-length)
               :let [opcode (read-byte)
                     instruction-map (get instructions opcode)]]
           (assoc (read-op instruction-map)
                  :op-code opcode
                  :mnemonic (:mnemonic instruction-map)
                  :offset offset
                  :nr     i)))))))

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

                                        ;(declare read-attributes)
(defn read-attributes [])

(defn read-code-attribute [input-stream]
  (binding [*byte-offset* (atom 0)
            *input-stream* input-stream]
    {:max-stack       (read-short)
     :max-locals      (read-short)
     :code            (read-code)
     :exception-table (read-exception-table)
     :attrs           (read-attributes)}))
