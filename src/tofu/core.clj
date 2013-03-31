(ns tofu.core
  (:gen-class)
  (:use [clojure.pprint :only [cl-format]])
  (:require [tofu.persistence :as persistence])
  (:import [jline console.ConsoleReader TerminalFactory]))

(def tasks (atom []))

(def console-reader (ConsoleReader.))
(def term (TerminalFactory/create))

(defn- read-char []
  (.setEchoEnabled term false)
  (let [c (char (.readCharacter console-reader))]
    (.setEchoEnabled term true)
    c))

(defn- print-tasks [tasks]
  (cl-format true "~:{~D. [ ] ~A~%~}" (map vector (range) (map :name tasks))))

(defn- print-welcome-banner []
  (cl-format true "Welcome to Tofu! You have ~:D task~:P to complete.~%~%" (count @tasks)))

(defn- add-task [name]
  (swap! tasks conj {:name name}))

(defn- choose-task [tasks]
  (loop []
    (print "Enter task number: ")
    (flush)
    ;; TODO handle NumberFormatException
    (let [idx (Integer/parseInt (read-line))]
      (newline)
      (if (and (> idx -1)
               (< idx (count tasks)))
        idx
        (do (println "Invalid task number.")
            (recur))))))

(defn- remove-nth [coll index]
  (into (subvec coll 0 index)
        (subvec coll (inc index) (count coll))))

(defn- delete-task [tasks task-index]
  (swap! tasks remove-nth task-index))

(defn- load-tasks []
  (if-let [loaded-tasks (persistence/load-tasks)]
    (reset! tasks loaded-tasks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-task-command []
  (println "What do you need to do?")
  (add-task (read-line)))

(defn- delete-task-command []
  ;; XXX Not thread safe
  (if-let [chosen-task-index (choose-task @tasks)]
    (delete-task tasks chosen-task-index)))

(defn- help-command []
  (println "You're not likely to get any help around here."))

(defn- print-command []
  (print-tasks @tasks))

(defn- save-command []
  (let [ts @tasks]
    (persistence/save-tasks ts)
    (cl-format true "Saved ~D task~:P to ~A.~%" (count ts) persistence/tasks-file-name)))

(def command-map
  {\a add-task-command
   \d delete-task-command
   \h help-command
   \p print-command
   \s save-command
   \? help-command})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- run-command-loop []
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
  (print-tasks @tasks)
  (newline)
  (run-command-loop)
  (save-command))
