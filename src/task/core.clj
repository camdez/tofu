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

(defn print-welcome-banner []
  (println "Welcome to Task!")
  (newline))

(defn run-command-loop []
  (print-tasks tasks)
  (let [command (read-line)]
    (if (not= command "q")
      (recur))))

(defn -main [& m]
  (print-welcome-banner)
  (run-command-loop))
