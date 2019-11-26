(ns class-analyzer.util
  (:import [java.io DataInputStream InputStream]))

(def ^:private ^java.nio.charset.Charset UTF-8 (java.nio.charset.Charset/forName "UTF-8"))

(defmacro str!
  "Throws AssertionError iff param is not a string."
  [x]
  `(doto ~x (assert ~(str "Not string: " (pr-str x)))))


(defn stream->str
  "Reads len bytes from an input stream. Returns UTF-8 string of bytes read."
  [^InputStream is len]
  (let [ba (byte-array len)]
    (loop [read-len 0]
      (if (= read-len len)
        (new String ba UTF-8)
        (recur (+ read-len (.read is ba read-len (- len read-len))))))))
