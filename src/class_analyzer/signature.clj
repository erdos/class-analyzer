(ns class-analyzer.signature
  "Recursive Descent Parser for JVM signatures."
  (:import [java.io Reader BufferedReader StringReader PushbackReader]))

(set! *warn-on-reflection* true)

(def ^:dynamic *reader* nil)

(defn- expect [^PushbackReader reader s]
  (assert (char? s))
  (let [r (.read reader)]
    (if (neg? r)
      nil
      (if (= (char r) (char s))
        s
        (do (.unread reader r)
            nil)))))

;; returns a read identifier or nil
(defn identifier
  "Reads a Java identifier as a String from the Reader. Returns nil and reverts when not found."
  [^PushbackReader reader]
  (let [first-char (.read reader)]
    (when (neg? first-char)
      (throw (ex-info "Unexpected end of stream!" {})))
    (if (Character/isJavaIdentifierStart first-char)
      (let [sb (new StringBuilder)]
        (.append sb (char first-char))
        (loop []
          (let [next-char (.read reader)]
            (if (Character/isJavaIdentifierPart next-char)
              (do
                (.append sb (char next-char))
                (recur))
              (do (.unread reader next-char)
                  (str sb))))))
      (do (.unread reader first-char) nil))))

(identifier (new PushbackReader (new StringReader " sdf")))

(defn- either [reader & reader-functions]
  (some #(% reader) reader-functions))

(defn- wildcard-indicator [reader]
  (or (and (expect reader \+) :super)
      (and (expect reader \-) :extends)))

(defn- expect! [^PushbackReader reader s]
  (assert (expect reader s)
          (str "Could not read " s ", suffix: " (slurp reader))))

(defn- separated-tokens-1 [^PushbackReader reader separator-char reader-fn]
  (when-let [fst (reader-fn reader)]
    (loop [result [fst]]
      (if (expect reader separator-char)
        (if-let [nxt (reader-fn reader)]
          (recur (conj result nxt))
          (throw (ex-info "Could not read" {:reader-fn reader-fn})))
        result))))

(defn- repeat+ [^PushbackReader reader reader-fn]
  (when-let [fst (reader-fn reader)]
    (loop [result [fst]]
      (if-let [nxt (reader-fn reader)]
        (recur (conj result nxt))
        result))))

(defn- repeat* [^PushbackReader reader reader-fn]
  (vec (repeat+ reader reader-fn)))

(defn- wrapped [reader before-char inside-fn after-char]
  (when (expect reader before-char)
    (let [result (inside-fn reader)]
      (assert result)
      (assert (expect reader after-char) (str "rest: " (slurp reader)))
      result)))

(defn- class-bound [reader]
  (when (expect reader \:)
    (field-type-signature reader))) ;; optional

(defn- interface-bound [reader]
  (when (expect reader \:)
    (doto (field-type-signature reader)
      (assert)))) ;; not optional.

;; identifier classbound interfacebound*
(defn- formal-type-parameter [^Reader reader]
  (when-let [id (identifier reader)]
    (let [classbound (class-bound reader)
          interfacebound (repeat* reader interface-bound)]
      {:identifier identifier
       :clsasbound classbound
       :interfacebound interfacebound})))

;; < FormalTypeParameter+ >
(defn- formal-type-parameters [^Reader reader]
  (wrapped reader \< #(repeat+ % formal-type-parameter) \>))

(declare class-type-signature array-type-signature type-variable-signature type-signature base-type)

(defn- base-type [reader]
  (either reader
          #(and (expect % \B) :byte)
          #(and (expect % \C) :char)
          #(and (expect % \D) :double)
          #(and (expect % \F) :float)
          #(and (expect % \I) :int)
          #(and (expect % \J) :long)
          #(and (expect % \S) :short)
          #(and (expect % \Z) :boolean)))

;; L - reference
;; [ - array of one dimension

(defn- type-variable-signature [reader]
  (wrapped reader \T identifier \;))

(defn- field-type-signature [reader]
  (either reader
          class-type-signature
          array-type-signature
          type-variable-signature))

(defn- type-signature [reader] (either reader field-type-signature base-type))

(defn- array-type-signature [reader]
  (when (expect reader \[)
    (let [ts (type-signature reader)]
      (assert ts)
      {:array ts})))

;; XXX itt a gond!!
(defn- class-type-signature [reader]
  (letfn [(type-argument [reader]
            (either reader
                    #(expect % \*)
                    #(let [ind? (wildcard-indicator %)
                           fts  (field-type-signature %)]
                       (when ind? (assert fts))
                       (when fts
                         {:indicator ind?
                          :field-type-signature fts}))))
          (type-arguments [reader]
            (wrapped reader \< #(repeat+ % type-argument) \>))]
    (when (expect reader \L)
      (let [pkgs+id  (separated-tokens-1 reader \/ identifier)
            pkgs     (pop pkgs+id)
            id       (peek pkgs+id)
            id-type? (type-arguments reader) ;; opt
            sufs (repeat* reader
                          #(when (expect % \.)
                             (hash-map :id (identifier %)
                                       :type-arg (type-argument %))))]
        (expect! reader \;)
        (cond->
            {:package (clojure.string/join "." pkgs)
             :class id}
          id-type? (assoc :generic id-type?)
          (seq sufs) (assoc :sufs sufs))))))


(defn- super-class-signature [^Reader reader] (class-type-signature reader))
(defn- super-interface-signature [^Reader reader] (class-type-signature reader))

(defn class-signature
  "FormalTypeParameters_opt SuperclassSignature SuperinterfaceSignature*"
  [^Reader reader]
  (let [fts        (formal-type-parameters reader)]
    (if-let [superclass (super-class-signature reader)]
      (let [is (repeat* reader super-interface-signature)]
        {:formal-type-parameters fts
         :superclass superclass
         :superinterface is})
      (if fts (assert false)))))



#_
(class-signature
 (PushbackReader. (StringReader. "<T:Ljava/lang/Object;>Ljava/lang/Object;Ljava/lang/Iterable<TT;>;")))
