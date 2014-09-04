(ns aprint.tty
  (:import [jline.console ConsoleReader]))

(defn- console []
  (ConsoleReader.))

(defn- terminal []
  (.getTerminal (console)))

(defn tty-width []
  (.getWidth (terminal)))

(defn tty-height []
  (.getHeight (terminal)))

(defn- fn-tty-area []
  (* (tty-width) (tty-height)))

(def tty-area (memoize fn-tty-area))

(defn- fn-ansi-supported? []
  (.isAnsiSupported (terminal)))

(def ansi-supported? (memoize fn-ansi-supported?))

(defn clear-screen []
  (.clearScreen (console)))

