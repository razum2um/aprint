(ns aprint.dispatch
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [aprint.utils :refer :all])
  (:import [java.io Writer]))

(def ^:dynamic *aprint-records* true)
(def ^:dynamic *aprint-seq-length* 10)
(def ^:dynamic *aprint-map-length* nil)

(defn- pprint-vector [avec]
  (binding [*print-length* *aprint-seq-length*]
    (pprint-logical-block :prefix (style "[" :magenta) :suffix (style "]" :magenta)
                          (print-length-loop [aseq (seq avec)]
                                             (when aseq
                                               (write-out (first aseq))
                                               (when (next aseq)
                                                 (.write ^java.io.Writer *out* " ")
                                                 (pprint-newline :fill)
                                                 (recur (next aseq))))))))


;; colorized brackets - sign that map has been sorted
(defn- pprint-map [amap colorize-brackets?]
  (let [color (first (shuffle random-brackets-style))
        prefix (if colorize-brackets? (style "{" color) "{")
        suffix (if colorize-brackets? (style "}" color) "}")]
    (pprint-logical-block :prefix (style prefix color) :suffix (style suffix color)
                          (binding [*print-length* *aprint-map-length*]
                            (print-length-loop [aseq (seq amap)]
                                               (when aseq
                                                 (let [k (ffirst aseq)]
                                                   (if (string? k)
                                                     (write-out (style (without-ansi k) :green))
                                                     (write-out k)))
                                                 (.write ^java.io.Writer *out* " ")
                                                 (write-out (fnext (first aseq)))
                                                 (when (next aseq)
                                                   (.write ^java.io.Writer *out* ", ")
                                                   (if (and (complex? (next aseq))
                                                            (complex? (fnext aseq))
                                                            (complex? (second (fnext aseq))))
                                                     (pprint-newline :linear)
                                                     (pprint-newline :fill)
                                                     )
                                                   (recur (next aseq)))))))))

(defn- pprint-map-sorted [amap]
  (try
    (pprint-map (into (sorted-map) amap) true)
    (catch ClassCastException e (pprint-map amap false))))

(defn- pprint-simple-list [alis]
  (binding [*print-length* *aprint-seq-length*]
    (let [color (first (shuffle random-brackets-style))]
      (pprint-logical-block :prefix (style "(" color) :suffix (style ")" color)
                            (print-length-loop [alis (seq alis)]
                                               (when alis
                                                 (write-out (first alis))
                                                 (when (next alis)
                                                   (.write ^java.io.Writer *out* " ")
                                                   (pprint-newline :linear)
                                                   (recur (next alis)))))))))

(defn- pprint-list [alis]
  (if-not (#'pprint/pprint-reader-macro alis)
    (pprint-simple-list alis)))

(defn- pprint-record [r]
  (if *aprint-records*
    (do
      (.write *out* "#")
      (.write *out* (.getSimpleName (class r)))
      (pprint-map-sorted r))
    (pprint-map-sorted r)))

(defmulti color-dispatch (fn [x]
                           (let [t (get (meta x) :type)]
                             (if (keyword? t) t (class x)))))

(use-method color-dispatch clojure.lang.IRecord pprint-record)
(use-method color-dispatch clojure.lang.IPersistentMap pprint-map-sorted)
(use-method color-dispatch clojure.lang.IPersistentVector pprint-vector)
(use-method color-dispatch clojure.lang.ISeq pprint-list)
(use-method color-dispatch clojure.lang.Keyword (partial raw-color-pprint :green))
(use-method color-dispatch java.lang.Long (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.Integer (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.Double (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.Boolean (partial raw-color-pprint :cyan))
(use-method color-dispatch java.lang.String (partial color-pprint :yellow))
(use-method color-dispatch nil pr)
(use-method color-dispatch :default pprint/simple-dispatch)

(prefer-method color-dispatch clojure.lang.IRecord clojure.lang.IPersistentMap)

