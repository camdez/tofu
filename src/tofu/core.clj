(ns tofu.core
  (:gen-class)
  (:use [clojure.pprint :only [cl-format]])
  (:require [tofu.io          :as io]
            [tofu.persistence :as persistence]
            [tofu.utils       :as u]))

(defn- print-task [t idx number-col-width]
  (cl-format true "~VD. [~:[ ~;X~]] ~A~:[~; <--~]~%"
             number-col-width idx (:completed t) (:name t) (:priority t)))

(defn- print-tasks [tasks]
  (let [number-col-width (-> tasks count Math/log10 Math/ceil int)]
    (doall (map #(print-task %1 %2 number-col-width) tasks (range))))
  (newline) (flush))

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

(defn- mark-task-done [task]
  (assoc task :completed (java.util.Date.)))

(defn- mark-task-undone [task]
  (assoc task :completed false))

(defn- toggle-task-done [task]
  (let [done? (:completed task)
        cmd   (if done? mark-task-undone mark-task-done)]
    (cmd task)))

(defn- toggle-task-priority [task]
  (update-in task [:priority] not))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-world {:tasks [], :opts {:sort-mode 0}})

(defn- print-welcome-banner-command [w]
  (cl-format true "Welcome to Tofu! You have ~:D task~:P to complete.~%~%"
             (->> w :tasks (remove :completed) count))
  w)

(def clear-screen-command
  (u/pass (io/clear-screen)))

(defn- add-task-command [{:keys [tasks] :as w}]
  (println "What do you need to do?")
  (assoc w :tasks (add-task tasks (read-line))))

(defn load-tasks-command [w]
  (let [tasks (or (persistence/load-tasks) [])]
    (assoc w :tasks tasks)))

(defn- delete-task-command [{:keys [tasks] :as w}]
  (if-let [t (choose-task w)]
    (assoc w :tasks (u/remove-el t tasks))
    w))

(def help-command
  (u/pass (println "You're not likely to get any help around here.")))

(defn- toggle-done-command [{:keys [tasks] :as w}]
  (if-let [t (choose-task w)]
    (assoc w :tasks (u/replace-el t (toggle-task-done t) tasks))
    w))

(defn- toggle-priority-command [{:keys [tasks] :as w}]
  (if-let [t (choose-task w)]
    (assoc w :tasks (u/replace-el t (toggle-task-priority t) tasks))
    w))

(defn- filter-tasks
  "Filter tasks according to options.  Returns the filtered list of
  tasks (not a world)."
  [tasks opts]
  (->> tasks
       ((if (:hide-done opts) (partial remove :completed) identity))
       ((if-let [re (:name-filter opts)] (partial filter #(re-find re (:name %))) identity))))

(def sort-fns
  [identity
   (partial sort-by :name)
   (partial sort-by :created)
   (partial sort-by #(or (:completed %) (java.util.Date. 0)))])

(defn- sort-tasks
  "Sort tasks according to options.  Returns the sorted list of
  tasks (not a world)."
  [tasks opts]
  (let [fn-idx  (get opts :sort-mode 0)
        sort-fn (nth sort-fns fn-idx)]
    (sort-fn tasks)))

(defn- print-command [{:keys [tasks opts] :as w}]
  (let [ftasks (sort-tasks (filter-tasks tasks opts) opts)
        w2     (assoc-in w [:tmp :ftasks] ftasks)]
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
             #(when-not % (io/read-regex "Enter filter regex"))))

(defn- cycle-sort-fn-command [w]
  (update-in w [:opts :sort-mode]
             (fnil #(mod (inc %) (count sort-fns))
                   -1)))

(defn- quit-command [w]
  (assoc-in w [:tmp :quit] true))

(def command-map
  {\a add-task-command
   \d delete-task-command
   \D toggle-filter-done-command
   \q quit-command
   \h help-command
   \l clear-screen-command
   \p print-command
   \s save-command
   \+ toggle-debug-command
   \t toggle-done-command
   \. cycle-sort-fn-command
   \/ toggle-regex-filter
   \* toggle-priority-command
   \? help-command})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- run-command-loop [{:keys [opts] :as w}]
  (if (get-in w [:tmp :quit])
    w
    (do
      (when (:debug opts)
        (println "State:" w))
      (print "Command: ") (flush)
      (let [command-char (io/read-char)]
        (println command-char) (newline) (flush)
        (if-let [command (get command-map command-char)]
          (recur (command w))
          (do
            (println "Invalid command.")
            (recur w)))))))

(defn -main [& m]
  (->> default-world
       load-tasks-command
       print-welcome-banner-command
       print-command
       run-command-loop
       save-command)
  (shutdown-agents))
