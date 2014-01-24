(ns tofu.utils)

(defmacro pass [& body]
  `(fn [w#]
     ~@body
     w#))

(defn remove-el [el s]
  (filterv (partial not= el) s))

(defn replace-el [el nel s]
  (replace {el nel} s))
