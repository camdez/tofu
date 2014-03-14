(ns tofu.io
  (:use [clojure.java.shell :only [sh]]))

(defn- shell [cmd]
  (sh "/bin/sh" "-c" cmd))

(defn- stty
  "Issues commands to control the underlying terminal (see `man
stty`). Makes a lot of assumptions as written."
  [command]
  (shell (str "stty " command " < /dev/tty")))

(defn clear-screen []
  (shell "tput clear > /dev/tty"))

(defn read-char []
  "Read a single character from the terminal without waiting for the
user to press RETURN."
  (stty "-echo -icanon")
  (let [ch (char (.read *in*))]
    (stty "echo icanon")
    ch))

(defn read-regex [msg]
  (print (str msg ": ")) (flush)
  (try (re-pattern (read-line))
       (catch java.util.regex.PatternSyntaxException e)))

(def grey-color  "\033[90m")
(def red-color   "\033[0;31m")
(def reset-color "\033[0m")
