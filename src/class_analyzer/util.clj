(ns class-analyzer.util
  (:import [java.io DataInputStream InputStream]))

(def ^:private ^java.nio.charset.Charset UTF-8 (java.nio.charset.Charset/forName "UTF-8"))

(defmacro str! [x] `(doto ~x (assert ~(str "Not string: " (pr-str x)))))

(defn stream->str [^InputStream is len]
  (let [ba (byte-array len)]
    (loop [read-len 0]
      (if (= read-len len)
        (new String ba UTF-8)
        (recur (+ read-len (.read is ba read-len (- len read-len))))))))

(defmacro with-input-stream [input-stream & bodies]

  )

;; returns a tuple of an atom containing the current byte count and a wrapped input-stream
(defn wrap-counted [input-stream]
  (let [a (atom 0)
        m (atom -1)]
    [a
     (proxy [InputStream] []
       (markSupported [] (.markSupported input-stream))
       (available [] (.available input-stream))
       (skip [n] (let [s (.skip input-stream n)]
                   (when (pos? s) (swap! a + s))
                   s))
       (mark [read-limit]
         (.mark input-stream read-limit)
         (reset! m @a))
       (reset []
         (.reset input-stream)
         (reset! a @m))
       (close [] (.close input-stream))
       (read
         ([]
          (let [s (.read input-stream)]
            (when (pos? s) (swap! a + s))
            s))
         ([^bytes bytes]
          (let [s (.read input-stream bytes)]
            (when (pos? s) (swap! a + s))
            s))
         ([^bytes bytes off len]
          (let [s (.read input-stream bytes off len)]
            (when (pos? s) (swap! a + s))
            s))))]))
