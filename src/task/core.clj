(ns task.core
  (:gen-class))

(import 'java.util.Date)

(def tasks [
            {:name "Do laundry"
             :due (Date.)},
            {:name "Write code"}
            ])

(defn print-task [task index]
  (println (str index ". [ ] " (:name task)))
  (if (contains? task :due)
    (println (str (:due task)))))

(defn print-tasks [tasks]
  (doseq [[task index] (map vector
                            tasks
                            (range))]
    (print-task task index)))

(defn -main [& m]
  (println "Welcome to Task!")
  (newline)
  (print-tasks tasks))
