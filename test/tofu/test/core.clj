(ns tofu.test.core
  (:use [tofu.core])
  (:use [clojure.test]))

(deftest delete-task-can-delete-at-beginning
  (is (= (delete-task [1 2 3] 0)
         [2 3])))

(deftest delete-task-can-delete-at-middle
  (is (= (delete-task [1 2 3] 1)
         [1 3])))

(deftest delete-task-can-delete-at-end
  (is (= (delete-task [1 2 3] 2)
         [1 2])))
