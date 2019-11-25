(ns class-analyzer.main
  (:require [class-analyzer.core :as core]
            [class-analyzer.javap :as javap]
            [class-analyzer.jar :as jar]
            [clojure.java.io :as io]))

(defn- parse-args [args]
  (loop [args args
         opts {}]
    (if-let [[a & args] (seq args)]
      (case a
        "-public"         (recur args (assoc opts :level :public))
        "-protected"      (recur args (assoc opts :level :protected))
        "-package"        (recur args (assoc opts :level :package))
        ("-p" "-private") (recur args (assoc opts :level :private))

        ;; show final constants
        "-constants"      (recur args (assoc opts :constants true))

        ;; disassemble the code
        "-c"              (recur args (assoc opts :disassemble true))

        ;; print internal type signatures
        "-s"              (recur args (assoc opts :signatures true))

        ;; print additional information
        ("-v" "-verbose") (recur args (assoc opts :verbose true))

        ;; print line number and local variable tables
        "-l"              (recur args (assoc opts :lineno+vartables true))

        ;; version info in the front
        ("-version")      (recur args (assoc opts :version? true))

        ;; arg separator
        "--"              (assoc opts :files args)

        ;; default case: list of files
        (assoc opts :files (cons a args)))
      opts)))

(defn main [& args]
  (let [parsed (parse-args args)]

    (when (:version parsed)
      (println "unknown version :-)"))

    (binding [javap/*level* (:level parsed :public)
              javap/*print-code* (:disassemble parsed false)
              javap/*print-signatures* (:signatures parsed false)]
      (doseq [file (:files parsed)
              :let [class-stream (io/input-stream (io/file file))
                    parsed (core/read-class class-stream)]]
        (javap/render parsed)))))
