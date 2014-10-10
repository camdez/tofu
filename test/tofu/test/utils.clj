(ns tofu.test.utils
  (:use [tofu.utils])
  (:use [clojure.test]))

(deftest remove-el-works
  (is (= (remove-el 0 [0 1 0 2 3 0])
         [1 2 3])))

(deftest replace-el-works
  (is (= (replace-el 1 0 [1 2 1 3])
         [0 2 0 3])))
