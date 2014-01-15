(ns tofu.persistence
  (:import [java.io File])
  (:require [clojure.tools.reader.edn :as edn]))

(def tasks-file-name (str (System/getProperty "user.home") "/.tofu/tasks.txt"))

(defn load-tasks []
  (when (.exists (File. tasks-file-name))
    (edn/read-string (slurp tasks-file-name))))

(defn save-tasks [tasks]
  (let [tofu-dir (.getParentFile (File. tasks-file-name))]
    (if-not (.exists tofu-dir)
      (.mkdirs tofu-dir)))
  (spit tasks-file-name (pr-str tasks)))
