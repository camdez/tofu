(ns tofu.core
  (:gen-class)
  (:require [clojure.pprint :refer :all]
            [clojure.string :as s]
            [tofu.io :as io]
            [tofu.persistence :as persistence]
            [tofu.utils :as u])
  (:import java.util.Date
           java.util.regex.PatternSyntaxException))

(defn- print-task [t idx number-col-width show-ages]
  (let [now (java.util.Date.)]
    (cl-format true "~VD. ~A[~:[ ~;X~]] ~A~A~A~%"
               number-col-width idx
               (if (:priority t) io/red-color "")
               (:completed t) (:name t)
               (if (:priority t) io/reset-color "")
               ;; TODO use num-col-width below
               (if show-ages
                 (str io/grey-color
                      "\n          Created: " (u/time-ago-description now (:created t))
                      (when (:completed t)
                        (str "\n        Completed: " (u/time-ago-description now (:completed t))))
                      io/reset-color)
                 ""))))

(defn- print-tasks [tasks opts]
  (when (seq tasks)
    (let [number-col-width (-> tasks count Math/log10 Math/ceil int)
          show-ages (:show-ages opts)]
      (doall (map #(print-task %1 %2 number-col-width show-ages) tasks (range))))
   (newline)) (flush))

(defn- add-task [tasks name]
  (conj tasks
        {:name name, :created (java.util.Date.), :completed false}))

(defn- choose-task [w]
  (if-let [tasks (get-in w [:tmp :filtered :tasks])]
    (loop []
      (print "Enter task number: ")
      (flush)
      (let [idx (try
                  (Integer/parseInt (read-line))
                  (catch NumberFormatException e))]
        (newline)
        (if idx
          (if (< -1 idx (count tasks))
            (nth tasks idx)
            (do (println "Invalid task number.")
                (recur)))
          (do
            (println "Nevermind.")
            nil))))
    (do (println "Please print tasks before attempting this command.")
        nil)))

(defn- with-task [w f]
  "Takes a function of two parameters that will be called with the
  world and a task if the user supplies one, else it will not be
  called."
  (if-let [t (choose-task w)]
    (f w t)
    w))

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

(defn- toggle-option [w key name]
  (update-in w [:opts key]
             #(let [new-val (not %)]
                (println (str name " " (if new-val "en" "dis") "abled."))
                new-val)))

(defn- cycle-option [w key values name]
  (let [w2 (update-in w [:opts key]
                      (fnil #(mod (inc %) (count values))
                            -1))]
    (println (str name " changed to " (:name (nth values (get-in w2 [:opts key]))) "."))
    w2))

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
  (let [new-name (read-line)]
    (if (s/blank? new-name)
      (do (println "Nevermind.")
          w)
      (assoc w :tasks (add-task tasks new-name)))))

(defn load-data-command [w]
  (merge w (persistence/load-data)))

;; Could definitely make this prettier with either a macro or a
;; function to define this function, but this is an improvement on the
;; general format of these commands because it cancels cleanly if a
;; task is not selected.
(defn- delete-task-command [w]
  (with-task w
    (fn [{:keys [tasks] :as w} t]
      (assoc w :tasks (u/remove-el t tasks)))))

(defn- edit-task-command [{:keys [tasks] :as w}]
  (if-let [t (choose-task w)]
    (do (println (str "Enter a new task name to replace '" (:name t) "':"))
        (let [new-name (read-line)]
          (if (s/blank? new-name)
            (do (println "Nevermind.")
                w)
            (assoc w :tasks (u/replace-el t (assoc t :name new-name) tasks)))))
    w))

(declare command-map)

(def help-command
  (u/pass
    (println "Command Index:") (newline)
    (doall (map (fn [[key h]]
                  (println (str key " --- " (:name h))))
                command-map))
    (newline)))

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
       ((if-let [re-str (:name-filter opts)] (partial filter #(re-find (re-pattern re-str) (:name %))) identity))))

(def sort-fns
  [{:name "manual"          :fn identity}
   {:name "name"            :fn (partial sort-by :name)}
   {:name "creation date"   :fn (partial sort-by :created)}
   {:name "completion date" :fn (partial sort-by #(or (:completed %) (java.util.Date. 0)))}
   {:name "priority"        :fn (partial sort-by (complement :priority))}])

(defn- sort-tasks
  "Sort tasks according to options.  Returns the sorted list of
  tasks (not a world)."
  [tasks opts]
  (let [fn-idx  (get opts :sort-mode 0)
        sort-fn (comp
                 (if (:reverse-sort opts) reverse identity)
                 (:fn (nth sort-fns fn-idx)))]
    (sort-fn tasks)))

(defn- print-command [{:keys [tasks opts] :as w}]
  (let [[f-tasks w2] (if (= (get-in w [:tmp :filtered :base]) [tasks opts])
                       ;; If we have the same tasks and filters as
                       ;; last time, just return.
                       [(get-in w [:tmp :filtered :tasks])
                        w]
                       ;; Else calculate and cache.
                       (let [ft (-> tasks
                                    (filter-tasks opts)
                                    (sort-tasks opts))]
                         [ft
                          (assoc-in w [:tmp :filtered] {:base [tasks opts]
                                                        :tasks ft})]))]
    (print-tasks f-tasks opts)
    (let [f-tasks-count (count f-tasks)
          tasks-count   (count tasks)]
      (when (< f-tasks-count tasks-count)
        (printf "(%d/%d)\n\n" f-tasks-count tasks-count)))
    w2))

(defn- today-command
  "Display tasks done today."
  [{:keys [tasks opts] :as w}]
  (do
    (let [horizon    (Date. (- (.getTime (Date.)) (* 1000 60 60 24) ))
          done-tasks (as-> tasks $
                       (filter #(when-let [completion-date (:completed %)]
                                  (.after completion-date horizon))
                               $)
                       (sort-tasks $ opts))]
      (if (seq done-tasks)
        (do
          (print (format "%d task(s) done in the last 24 hours:\n\n" (count done-tasks)))
          (print-tasks done-tasks opts))
        (println "No tasks done in the last 24 hours.")))
    w))

(defn- save-command [{:keys [tasks opts] :as w}]
  (persistence/save-data tasks opts true)
  (cl-format true "Saved ~D task~:P to ~A.~%" (count tasks) persistence/tasks-file-name)
  w)

(defn- toggle-show-ages-command [w]
  (toggle-option w :show-ages "Task age display"))

(defn- toggle-debug-command [w]
  (toggle-option w :debug "Debugging"))

(defn- toggle-filter-done-command [w]
  (toggle-option w :hide-done "Done task filtering"))

(defn- toggle-regex-filter [w]
  (print "Enter filter regex: ")
  (flush)
  (let [filter-str (read-line)]
    (if (= "" filter-str)
      (if (get-in w [:opts :name-filter])
        (do (println "Cleared filter.")
            (assoc-in w [:opts :name-filter] nil))
        (do (println "Nevermind.")
            w))
      (if (try (re-pattern filter-str)
               (catch PatternSyntaxException e))
        (assoc-in w [:opts :name-filter] filter-str)
        (do (println "Invalid regular expression.")
            w)))))

(defn- cycle-sort-fn-command [w]
  (cycle-option w :sort-mode sort-fns "Sort order"))

(defn- toggle-reverse-sort-command [w]
  (toggle-option w :reverse-sort "Reverse sort order"))

(defn- choose-register [w]
  (let [rs (get-in w [:tmp :registers])]
    (if (empty? rs)
      (println "No registers assigned.")
      (do (println (format "Choose register [%s]:"
                           (-> rs keys sort s/join)))
          (let [r (io/read-char)]
            (when-not (= r \newline)
              (or (get rs r)
                  (do (println "Invalid register.")
                      (recur w)))))))))

(defn- load-opts-from-register-command [w]
  (if-let [opts (choose-register w)]
    (do (println "Loaded configuration.")
        (assoc w :opts opts))
    (do (println "Nevermind.")
        w)))

(defn- save-opts-to-register-command [w]
  (println (format "Choose register [%s]:"
                   (-> w (get-in [:tmp :registers]) keys sort s/join)))
  (let [r (io/read-char)]
    (if (= r \newline)
      (do (println "Nevermind.")
          w)
      (do (println (format "Saved configuration to register %s." r))
          (assoc-in w [:tmp :registers r] (get w :opts))))))

(defn- quit-command [w]
  (assoc-in w [:tmp :quit] true))

(defn- undo-command [w]
  (if-let [old-w (-> w :tmp :undo)]
    (assoc-in old-w [:tmp :redo] w)
    (do (println "No more undo history!") w)))

(defn- redo-command [w]
  (if-let [new-w (-> w :tmp :redo)]
    (assoc-in new-w [:tmp :undo] w)
    (do (println "No more redo history!") w)))

(def command-map
  (reduce (fn [m [key sym]]
            (assoc m key {:name (name sym)
                          :fn   (-> sym resolve deref)}))
          {}
          '{\" save-opts-to-register-command
            \' load-opts-from-register-command
            \* toggle-priority-command
            \+ toggle-debug-command
            \. cycle-sort-fn-command
            \/ toggle-regex-filter
            \> toggle-reverse-sort-command
            \? help-command
            \A toggle-show-ages-command
            \D toggle-filter-done-command
            \a add-task-command
            \d delete-task-command
            \e edit-task-command
            \h help-command
            \l clear-screen-command
            \p print-command
            \q quit-command
            \r redo-command
            \s save-command
            \t toggle-done-command
            \T today-command
            \u undo-command}))

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
        (if-let [command (get-in command-map [command-char :fn])]
          (let [new-w (command w)]
            (if (or (= command undo-command)
                    (= (:tasks w) (:tasks new-w)))
              (recur new-w)
              (recur (assoc-in new-w [:tmp :undo] w))))
          (do
            (println "Invalid command.")
            (recur w)))))))

(defn -main [& m]
  (->> default-world
       load-data-command
       print-welcome-banner-command
       print-command
       run-command-loop
       save-command)
  (shutdown-agents))
