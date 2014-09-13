(ns aprint.dispatch
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [aprint.utils :refer :all])
  (:import [java.io Writer]))

(defn- pprint-vector [avec]
  (pprint-logical-block :prefix (style "[" :magenta) :suffix (style "]" :magenta)
                               (print-length-loop [aseq (seq avec)]
                                                         (when aseq
                                                           (write-out (first aseq))
                                                           (when (next aseq)
                                                             (.write ^java.io.Writer *out* " ")
                                                             (pprint-newline :linear)
                                                             (recur (next aseq)))))))


;; colorized brackets - sign that map has been sorted
(defn- pprint-map [amap colorize-brackets?]
  (let [color (first (shuffle random-brackets-style))
        prefix (if colorize-brackets? (style "{" color) "{")
        suffix (if colorize-brackets? (style "}" color) "}")]
    (pprint-logical-block :prefix (style prefix color) :suffix (style suffix color)
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
                                                 (recur (next aseq))))))))

(defn- pprint-map-sorted [amap]
  (try
    (pprint-map (into (sorted-map) amap) true)
    ;; workaround http://dev.clojure.org/jira/browse/CLJ-1466
    (catch AbstractMethodError e (pprint-map (into (sorted-map) (seq amap)) true))
    (catch ClassCastException e (pprint-map amap false))))

(defn- pprint-simple-list [alis]
  (let [color (first (shuffle random-brackets-style))]
    (pprint-logical-block :prefix (style "(" color) :suffix (style ")" color)
                          (print-length-loop [alis (seq alis)]
                                             (when alis
                                               (write-out (first alis))
                                               (when (next alis)
                                                 (.write ^java.io.Writer *out* " ")
                                                 (pprint-newline :linear)
                                                 (recur (next alis))))))))

(defn- pprint-list [alis]
  (if-not (#'pprint/pprint-reader-macro alis)
    (pprint-simple-list alis)))

(defmulti color-dispatch class)

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
