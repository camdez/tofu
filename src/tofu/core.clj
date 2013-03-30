(ns tofu.core
  (:gen-class)
  (:require [clojure.tools.reader.edn :as edn])
  (:import [jline console.ConsoleReader TerminalFactory]
           [java.io File]))

(def tasks-file-name (str (System/getProperty "user.home") "/.tofu/tasks.txt"))
(def tasks (atom []))

(def console-reader (ConsoleReader.))
(def term (TerminalFactory/create))

(defn read-char []
  (.setEchoEnabled term false)
  (let [c (char (.readCharacter console-reader))]
    (.setEchoEnabled term true)
    c))

(defn load-tasks []
  (when (.exists (File. tasks-file-name))
    (reset! tasks
            (edn/read-string (slurp tasks-file-name)))))

(defn print-task [task index]
  (println (str index ". [ ] " (:name task))))

(defn print-tasks [tasks]
  (doseq [[task index] (map vector
                            tasks
                            (range))]
    (print-task task index)))

(defn print-welcome-banner []
  (println "Welcome to Tofu!")
  (newline))

(defn add-task [name]
  (swap! tasks conj {:name name}))

(defn choose-task [tasks]
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

(defn remove-nth [coll index]
  (into (subvec coll 0 index)
        (subvec coll (inc index) (count coll))))

(defn delete-task [tasks task-index]
  (swap! tasks remove-nth task-index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-task-command []
  (println "What do you need to do?")
  (add-task (read-line)))

(defn delete-task-command []
  ;; XXX Not thread safe
  (if-let [chosen-task-index (choose-task @tasks)]
    (delete-task tasks chosen-task-index)))

(defn help-command []
  (println "You're not likely to get any help around here."))

(defn print-command []
  (print-tasks @tasks))

(defn save-command []
  (let [tofu-dir (.getParentFile (File. tasks-file-name))]
    (if-not (.exists tofu-dir)
      (.mkdirs tofu-dir)))
  (spit tasks-file-name @tasks)
  (printf "Saved %d task(s) to %s.\n" (count @tasks) tasks-file-name))

(def command-map
  {\a add-task-command
   \d delete-task-command
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
  (run-command-loop)
  (save-command))
