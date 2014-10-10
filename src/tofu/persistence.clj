(ns tofu.persistence
  (:import [java.io File])
  (:require [clojure.edn :as edn]))

(def tasks-file-name (str (System/getProperty "user.home") "/.tofu/tasks.txt"))

(defn load-tasks []
  (when (.exists (File. tasks-file-name))
    (let [data (edn/read-string (slurp tasks-file-name))]
      (if (map? data)
        (:tasks data)
        data ; v1 format was just a vector of tasks
        ))))

(defn save-tasks [tasks]
  (let [tofu-dir (.getParentFile (File. tasks-file-name))]
    (if-not (.exists tofu-dir)
      (.mkdirs tofu-dir)))
  (let [data {:version 2
              :tasks   tasks}]
    (spit tasks-file-name (pr-str data))))
