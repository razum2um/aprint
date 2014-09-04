(ns aprint.utils
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as s]
            [clansi.core :as clansi])
  (:import [java.io Writer]
           [jline.console ConsoleReader]))

(def random-brackets-style [:red :blue :cyan])

(declare style)

(defn- escaped-string [^String s]
  (apply str (map #(or (char-escape-string %) %) (char-array s))))

;; patch core to to append to WWriter by chars
(defmethod print-method String [^String s, ^Writer w]
  (if (or *print-dup* *print-readably*)
    (do (.append w \")
        (.append w (escaped-string s))
        (.append w \"))
    (.write w s))
  nil)

;; (defmethod print-method clojure.lang.ISeq [o, ^Writer w]
;;   (let [color (first (shuffle random-brackets-style))]
;;     (#'clojure.core/print-meta o w)
;;     (#'clojure.core/print-sequential (style "(" color) #'clojure.core/pr-on " " (style ")" color) o w)))
;;
;; (defmethod print-method java.util.List [c, ^Writer w]
;;   (if *print-readably*
;;     (let [color (first (shuffle random-brackets-style))]
;;       (#'clojure.core/print-meta c w)
;;       (#'clojure.core/print-sequential (style "(" color) #'clojure.core/pr-on " " (style ")" color) c w))
;;     (#'clojure.core/print-object 1 w)
;;     ))

;; (defmethod print-method java.util.RandomAccess [v, ^Writer w]
;;   (if *print-readably*
;;     (do
;;       (print-meta v w)
;;       (print-sequential "[" pr-on " " "]" v w))
;;     (print-object v w)))
;;
;; (defmethod print-method java.util.Set [s, ^Writer w]
;;   (if *print-readably*
;;     (do
;;       (print-meta s w)
;;       (print-sequential "#{" pr-on " " "}" (seq s) w))
;;     (print-object s w)))

(def use-method #'pprint/use-method)

(defn color-pprint [color obj]
  (pr (clansi/style obj color)))

(defn raw-color-pprint [color obj]
  (binding [*print-readably* false
            *print-dup* false]
    (color-pprint color obj)))

(defn scalar? [x]
  (or (number? x)
      (string? x)
      (instance? java.lang.Boolean true)
      (nil? x)))

(defn complex? [x]
  (not (scalar? x)))

(defmacro binding-map [amap & body]
  (let []
    `(do
       (. clojure.lang.Var (pushThreadBindings ~amap))
       (try
        ~@body
        (finally
         (. clojure.lang.Var (popThreadBindings)))))))

(defmacro wrapns [var wrapfn] `(alter-var-route #'~var ~wrapfn))

(defn- terminal []
  (.getTerminal (ConsoleReader.)))

(defn- fn-tty-width []
  (.getWidth (terminal)))

(def tty-width (memoize fn-tty-width))

(defn- fn-ansi-supported? []
  (.isAnsiSupported (terminal)))

(def ansi-supported? (memoize fn-ansi-supported?))

(defn style [s color]
  (if (ansi-supported?)
    (clansi/style s color)
    s))

;; "\e[\d+m"
(def ansi-code-pattern
  (re-pattern (reduce str (map char [27 92 91 92 100 43 109]))))

(defn without-ansi [s]
  (s/replace s ansi-code-pattern ""))

