(ns task.core
  (:gen-class))

(require '[clojure.tools.reader.edn :as edn])

(def tasks [
            {:name "Do laundry"}
            {:name "Write code"}
            ])

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
