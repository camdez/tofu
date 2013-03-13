(ns task.core
  (:gen-class))

(import 'java.util.Date)

(def tasks [
            {:name "Do laundry"
             :due (Date.)},
            {:name "Write code"}
            ])

(defn print-task [task]
  (println (str "[ ] " (:name task)))
  (if (contains? task :due)
    (println (str (:due task)))))

(defn print-tasks [tasks]
  (doseq [t tasks]
    (print-task t)))

(defn -main [& m]
  (println "Welcome to Task!")
  (newline)
  (print-tasks tasks))
