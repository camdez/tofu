(ns task.core
  (:gen-class)
  (:require [clojure.tools.reader.edn :as edn])
  (:import [jline console.ConsoleReader TerminalFactory]))

(def tasks-file-name "tasks.txt")
(def tasks
  (edn/read-string (slurp tasks-file-name)))

(def console-reader (ConsoleReader.))
(def term (TerminalFactory/create))

(defn read-char []
  (.setEchoEnabled term false)
  (let [c (char (.readCharacter console-reader))]
    (.setEchoEnabled term true)
    c))

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
  (def tasks (conj tasks {:name name})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-task-command []
  (println "What do you need to do?")
  (add-task (read-line)))

(defn help-command []
  (println "You're not likely to get any help around here."))

(defn print-command []
  (print-tasks tasks))

(def command-map
  {\a add-task-command
   \h help-command
   \p print-command
   \? help-command})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-command-loop []
  (print "Command: ")
  (flush)
  (let [command-char (read-char)]
    (println command-char)
    (when (not= command-char \q)
      (let [command (get command-map command-char)]
        (if command
          (eval (list command))
          (println "Invalid command.")))
      (newline)
      (recur))))

(defn -main [& m]
  (print-welcome-banner)
  (run-command-loop))
