(ns task.core
  (:gen-class))

(require '[clojure.tools.reader.edn :as edn])

(def tasks-file-name "tasks.txt")
(def tasks
  (edn/read-string (slurp tasks-file-name)))

(defn print-task [task index]
  (println (str index ". [ ] " (:name task))))

(defn print-tasks [tasks]
  (doseq [[task index] (map vector
                            tasks
                            (range))]
    (print-task task index)))

(defn -main [& m]
  (println "Welcome to Task!")
  (newline)
  (print-tasks tasks))
