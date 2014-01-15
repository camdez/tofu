(ns tofu.core
  (:gen-class)
  (:use [clojure.pprint :only [cl-format]]
        [clojure.java.shell :only [sh]])
  (:require [tofu.persistence :as persistence]))

(def tasks (atom []))

(defn- stty
  "Issues commands to control the underlying terminal (see `man
stty`). Makes a lot of assumptions as written."
  [command]
  (sh "/bin/sh" "-c" (str "stty " command " < /dev/tty")))

(defn- read-char []
  "Read a single character from the terminal without waiting for the
user to press RETURN."
  (stty "-echo -icanon")
  (let [ch (char (.read *in*))]
    (stty "echo icanon")
    ch))

(defn- print-tasks [tasks]
  (when (not-empty tasks)
    (let [number-col-width (-> tasks count Math/log10 Math/ceil int)]
      (cl-format true "~:{~VD. [~:[ ~;X~]] ~A~%~}"
                 (map vector (repeat number-col-width) (range) (map :completed tasks) (map :name tasks))))))

(defn- print-welcome-banner []
  (cl-format true "Welcome to Tofu! You have ~:D task~:P to complete.~%~%" (count @tasks)))

(defn- add-task [name]
  (swap! tasks conj {:name name, :created (java.util.Date.), :completed false}))

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

(defn- mark-task-done [tasks task-index]
  (swap! tasks (fn [ts idx]
                 (let [old-task (nth ts idx)
                       new-task (assoc old-task :completed (java.util.Date.))]
                   (assoc ts idx new-task)))
         task-index))

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

(defn- mark-done-command []
  ;; XXX Not thread safe
  (if-let [chosen-task-index (choose-task @tasks)]
    (mark-task-done tasks chosen-task-index)))

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
   \t mark-done-command
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
  (save-command)
  (shutdown-agents))
