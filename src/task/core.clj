(ns task.core
  (:gen-class)
  (:require [clojure.tools.reader.edn :as edn])
  (:import [jline console.ConsoleReader TerminalFactory]))

(def tasks-file-name "tasks.txt")
(def tasks (atom []))

(def console-reader (ConsoleReader.))
(def term (TerminalFactory/create))

(defn read-char []
  (.setEchoEnabled term false)
  (let [c (char (.readCharacter console-reader))]
    (.setEchoEnabled term true)
    c))

(defn load-tasks []
  (reset! tasks
          (edn/read-string (slurp tasks-file-name))))

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

(defn add-task [name]
  (swap! tasks conj {:name name}))

(defn delete-task [tasks task-index]
  (into (subvec tasks 0 task-index)
        (subvec tasks (inc task-index) (count tasks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-task-command []
  (println "What do you need to do?")
  (add-task (read-line)))

(defn help-command []
  (println "You're not likely to get any help around here."))

(defn print-command []
  (print-tasks @tasks))

(defn save-command []
  (spit tasks-file-name @tasks)
  (printf "Saved to %s.\n" tasks-file-name))

(def command-map
  {\a add-task-command
   \h help-command
   \p print-command
   \s save-command
   \? help-command})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-command-loop []
  (print "Command: ")
  (flush)
  (let [command-char (read-char)]
    (println command-char)
    (when (not= command-char \q)
      (if-let [command (get command-map command-char)]
        (eval (list command))
        (println "Invalid command."))
      (newline)
      (recur))))

(defn -main [& m]
  (load-tasks)
  (print-welcome-banner)
  (run-command-loop))
