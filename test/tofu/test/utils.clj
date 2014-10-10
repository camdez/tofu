(ns tofu.test.utils
  (:use [tofu.utils])
  (:use [midje.sweet]))

(facts "about `remove-el`"
  (remove-el 0 [0 1 0 2 3 0]) => [1 2 3]
  (remove-el 0 [1 2 3])       => [1 2 3])

(facts "about `replace-el`"
  (replace-el 1 0 [1 2 1 3]) => [0 2 0 3])
