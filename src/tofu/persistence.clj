(ns tofu.persistence
  (:import [java.io File])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def default-tasks-file-name
  (str (System/getProperty "user.home") "/.tofu/tasks.txt"))

(def tasks-file-name
  (or (System/getenv "TOFU_FILE")
      default-tasks-file-name))

(defn load-tasks []
  (when (.exists (File. tasks-file-name))
    (let [data (edn/read-string (slurp tasks-file-name))]
      (if (map? data)
        (:tasks data)
        data ; v1 format was just a vector of tasks
        ))))

;; The long-term goal for `pretty` would be print things in a diffable
;; way.  Hack for now.
(defn save-tasks [tasks pretty]
  (when (= tasks-file-name default-tasks-file-name)
    (let [tofu-dir (.getParentFile (File. tasks-file-name))]
      (if-not (.exists tofu-dir)
        (.mkdirs tofu-dir))))
  (let [data {:version 2
              :tasks   tasks}
        writer (io/writer tasks-file-name)]
    (if pretty
      (pp/pprint data writer)
      (spit writer (pr-str data)))))
