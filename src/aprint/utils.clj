(ns aprint.utils
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [clansi.core :as clansi])
  (:import [java.io Writer]))

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

(def use-method #'pprint/use-method)

(defn color-pprint [color obj]
  (pr (clansi/style obj color)))

(defn raw-color-pprint [color obj]
  (binding [*print-readably* false
            *print-dup* false]
    (color-pprint color obj)))

(defn scalar? [x]
  (or (number? x) (string? x) (nil? x)))

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

