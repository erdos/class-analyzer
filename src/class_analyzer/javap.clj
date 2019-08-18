(ns class-analyzer.javap
  (:require [class-analyzer.core :as cac]))


(comment

  (->>
   (cac/read-class
    (clojure.java.io/input-stream
     (clojure.java.io/file
      "/home/erdos/janos.erdos-coedit/classes/production/schema-lib-generator-java/com/prezi/homeassignment/schemalib/Vector.class"
      )))
   (clojure.pprint/pprint)
   )

  )

(defn main [& args])
