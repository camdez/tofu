(ns tofu.test.core
  (:use [tofu.core])
  (:use [clojure.test]))

(deftest remove-nth-can-remove-at-beginning
  (is (= (remove-nth [1 2 3] 0)
         [2 3])))

(deftest remove-nth-can-remove-at-middle
  (is (= (remove-nth [1 2 3] 1)
         [1 3])))

(deftest remove-nth-can-remove-at-end
  (is (= (remove-nth [1 2 3] 2)
         [1 2])))
