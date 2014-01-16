(ns tofu.core
  (:gen-class)
  (:use [clojure.pprint :only [cl-format]]
        [clojure.java.shell :only [sh]])
  (:require [tofu.persistence :as persistence]))

(defmacro pass [& body]
  `(fn [w#]
     ~@body
     w#))

(defn remove-el [el s]
  (filterv (partial not= el) s))

(defn replace-el [el nel s]
  (replace {el nel} s))

(defn- shell [cmd]
  (sh "/bin/sh" "-c" cmd))

(defn- stty
  "Issues commands to control the underlying terminal (see `man
stty`). Makes a lot of assumptions as written."
  [command]
  (shell (str "stty " command " < /dev/tty")))

(defn- clear-screen []
  (shell "tput clear > /dev/tty"))

(defn- read-char []
  "Read a single character from the terminal without waiting for the
user to press RETURN."
  (stty "-echo -icanon")
  (let [ch (char (.read *in*))]
    (stty "echo icanon")
    ch))

(defn- read-regex [msg]
  (print (str msg ": ")) (flush)
  (try (re-pattern (read-line))
       (catch java.util.regex.PatternSyntaxException e)))

(defn- print-tasks [tasks]
  (when (not-empty tasks)
    (let [number-col-width (-> tasks count Math/log10 Math/ceil int)]
      (cl-format true "~:{~VD. [~:[ ~;X~]] ~A~%~}"
                 (map vector (repeat number-col-width) (range) (map :completed tasks) (map :name tasks))))))

(defn- add-task [tasks name]
  (conj tasks
        {:name name, :created (java.util.Date.), :completed false}))

(defn- choose-task [w]
  (if-let [tasks (get-in w [:tmp :ftasks])]
    (loop []
      (print "Enter task number: ")
      (flush)
      ;; TODO handle NumberFormatException
      (let [idx (Integer/parseInt (read-line))]
        (newline)
        (if (< -1 idx (count tasks))
          (nth tasks idx)
          (do (println "Invalid task number.")
              (recur)))))
    (do (println "Please print tasks before attempting this command.")
        nil)))

(defn- remove-nth [coll index]
  (into (subvec coll 0 index)
        (subvec coll (inc index) (count coll))))

(defn- mark-task-done [task]
  (assoc task :completed (java.util.Date.)))

(defn- mark-task-undone [task]
  (assoc task :completed false))

(defn- toggle-task-done [task]
  (let [done? (:completed task)
        cmd   (if done? mark-task-undone mark-task-done)]
    (cmd task)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-world {:tasks [], :opts {}})

(defn- print-welcome-banner-command [w]
  (cl-format true "Welcome to Tofu! You have ~:D task~:P to complete.~%~%" (count (:tasks w)))
  w)

(def clear-screen-command
  (pass (clear-screen)))

(defn- add-task-command [{:keys [tasks] :as w}]
  (println "What do you need to do?")
  (assoc w :tasks (add-task tasks (read-line))))

(defn load-tasks-command [w]
  (let [tasks (or (persistence/load-tasks) [])]
    (assoc w :tasks tasks)))

(defn- delete-task-command [{:keys [tasks] :as w}]
  (if-let [t (choose-task w)]
    (assoc w :tasks (remove-el t tasks))
    w))

(def help-command
  (pass (println "You're not likely to get any help around here.")))

(defn- toggle-done-command [{:keys [tasks] :as w}]
  (if-let [t (choose-task w)]
    (assoc w :tasks (replace-el t (toggle-task-done t) tasks))
    w))

(defn- filter-tasks
  "Filter tasks according to options.  Returns the filtered list of
  tasks (not a world)."
  [tasks opts]
  (->> tasks
       ((if (:hide-done opts) (partial remove :completed) identity))
       ((if-let [re (:name-filter opts)] (partial filter #(re-find re (:name %))) identity))))

(defn- print-command [{:keys [tasks opts] :as w}]
  (let [ftasks (filter-tasks tasks opts)
        w2 (assoc-in w [:tmp :ftasks] ftasks)]
    (print-tasks ftasks)
    w2))

(defn- save-command [{:keys [tasks] :as w}]
  (persistence/save-tasks tasks)
  (cl-format true "Saved ~D task~:P to ~A.~%" (count tasks) persistence/tasks-file-name)
  w)

(defn- toggle-debug-command [w]
  (update-in w [:opts :debug] not))

(defn- toggle-filter-done-command [w]
  (update-in w [:opts :hide-done] not))

(defn- toggle-regex-filter [w]
  (update-in w [:opts :name-filter]
             #(when-not % (read-regex "Enter filter regex"))))

(def command-map
  {\a add-task-command
   \d delete-task-command
   \D toggle-filter-done-command
   \h help-command
   \l clear-screen-command
   \p print-command
   \s save-command
   \+ toggle-debug-command
   \t toggle-done-command
   \/ toggle-regex-filter
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
