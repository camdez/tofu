(ns tofu.core
  (:gen-class)
  (:use [clojure.pprint :only [cl-format]]
        [clojure.java.shell :only [sh]])
  (:require [tofu.persistence :as persistence]))

(defmacro pass [& body]
  `(fn [w#]
     ~@body
     w#))

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

(defn- add-task [tasks name]
  (conj tasks
        {:name name, :created (java.util.Date.), :completed false}))

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
  (remove-nth tasks task-index))

(defn- mark-task-done [task]
  (assoc task :completed (java.util.Date.)))

(defn- mark-task-undone [task]
  (assoc task :completed false))

(defn- toggle-task-done [tasks task-index]
  (let [task (nth tasks task-index)
        done? (:completed task)
        task' ((if done? mark-task-undone mark-task-done) task)]
    (assoc tasks task-index task')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-world {:tasks [], :opts {}})

(defn- print-welcome-banner-command [w]
  (cl-format true "Welcome to Tofu! You have ~:D task~:P to complete.~%~%" (count (:tasks w)))
  w)

(defn- add-task-command [{:keys [tasks] :as w}]
  (println "What do you need to do?")
  (assoc w :tasks (add-task tasks (read-line))))

(defn load-tasks-command [w]
  (let [tasks (or (persistence/load-tasks) [])]
    (assoc w :tasks tasks)))

(defn- delete-task-command [{:keys [tasks] :as w}]
  ;; XXX Not thread safe
  (if-let [chosen-task-index (choose-task tasks)]
    (assoc w :tasks (delete-task tasks chosen-task-index))
    w))

(def help-command
  (pass (println "You're not likely to get any help around here.")))

(defn- toggle-done-command [{:keys [tasks] :as w}]
  ;; XXX Not thread safe
  (if-let [chosen-task-index (choose-task tasks)]
    (assoc w :tasks (toggle-task-done tasks chosen-task-index))
    w))

(defn- print-command [{:keys [tasks] :as w}]
  (print-tasks tasks)
  w)

(defn- save-command [{:keys [tasks] :as w}]
  (persistence/save-tasks tasks)
  (cl-format true "Saved ~D task~:P to ~A.~%" (count tasks) persistence/tasks-file-name)
  w)

(defn- toggle-debug-command [w]
  (update-in w [:opts :debug] not))

(def command-map
  {\a add-task-command
   \d delete-task-command
   \h help-command
   \p print-command
   \s save-command
   \+ toggle-debug-command
   \t toggle-done-command
   \? help-command})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def quit-char \q)

(defn- run-command-loop [{:keys [opts] :as w}]
  (when (:debug opts)
    (println "State:" w))
  (print "Command: ")
  (flush)
  (let [command-char (read-char)]
    (println command-char)
    (if (= command-char quit-char)
      w
      (if-let [command (get command-map command-char)]
        (recur (command w))
        (do
          (println "Invalid command.")
          (recur w))))))

(defn -main [& m]
  (->> default-world
       load-tasks-command
       print-welcome-banner-command
       print-command
       run-command-loop
       save-command)
  (shutdown-agents))
