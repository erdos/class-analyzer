(ns class-analyzer.code
  (:require [class-analyzer.opcodes :refer :all]))

(def ^:private ^:dynamic ^java.io.DataInputStream *input-stream*)
(def ^:private ^:dynamic *byte-offset*) ;; long ref

(defn- read-byte []
  (swap *byte-offset* inc)
  (.readByte *input-stream*))

(defn- read-short []
  (swap *byte-offset* + 2)
  (.readShort *input-stream*))

(defn- read-int []
  (swap *byte-offset* + 4)
  (.readInt *input-stream*))

(defmulti ^:pivate read-op (fn [instruction-map] (:mnemonic instruction-map)))

;;
(defmethod read-op :default [{:keys [mnemonic args]}]
  (assert (vector? args))
  (let [xs (for [a args]
             (case a
               :byte (read-byte)
               :int (read-int)
               :short (read-short)))]
    {:args (vec xs)}))

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

(defn- read-code [^java.io.DataInputStream input-stream]
  (binding [*byte-offset* (atom 0)
            *input-stream* input-stream
            ops-count      ]
    (reset! *byte-offset* 0)
    (doall
     (for [i (range ops-count)
           :let [opcode (read-byte)
                 instruction-map (get instructions opcode)]]
       (-> (read-op instruction-map)
           (assoc :op-code op-code)
           (assoc :mnemonic (:mnemonic instruction-map))
           (assoc :offset @*byte-offset*)
           (assoc :nr     i))))))

(defn read-exception-table []
  (doall
   (for [i (range (read-short))]
     {:start-pc   (read-short)
      :end-pc     (read-short)
      :handler-pc (read-short)
      :catch-type (read-short)})))

(defn read-code-attribute [input-stream]
  (binding [*input-stream* input-stream]
    {:max-stack       (read-short)
     :max-locals      (read-short)
     :code            (read-code)
     :exception-table (read-exception-table)
     :attrs           (read-attributes)}))
