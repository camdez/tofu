(ns tofu.utils)

(defmacro pass [& body]
  `(fn [w#]
     ~@body
     w#))

(defn remove-el [el s]
  (filterv (partial not= el) s))

(defn replace-el [el nel s]
  (replace {el nel} s))

(defn time-ago-description [now-date then-date]
  (let [delta-secs (-> now-date .getTime (- (.getTime then-date)) (/ 1000) int)]
    (condp > delta-secs
      1    "just now"
      60    (str delta-secs " second(s) ago")
      3600  (str (-> delta-secs (/ 60) int) " minute(s) ago")
      86400 (str (-> delta-secs (/ 3600) int) " hour(s) ago")
            (str (-> delta-secs (/ 86400) int) " day(s) ago"))))
