(ns aprint.dispatch
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [clansi.core :as clansi]
            [aprint.utils :refer :all])
  (:import [java.io Writer]))

(defn- pprint-vector [avec]
  (pprint-logical-block :prefix (clansi/style "[" :magenta) :suffix (clansi/style "]" :magenta)
                               (print-length-loop [aseq (seq avec)]
                                                         (when aseq
                                                           (write-out (first aseq))
                                                           (when (next aseq)
                                                             (.write ^java.io.Writer *out* " ")
                                                             (pprint-newline :linear)
                                                             (recur (next aseq)))))))


(defn- pprint-map [amap]
  (let [color (first (shuffle [:red :blue :cyan]))]
  (pprint-logical-block :prefix "{" :suffix "}"
    (print-length-loop [aseq (seq amap)]
      (when aseq
            (write-out (ffirst aseq))
            (.write ^java.io.Writer *out* " ")
            (write-out (fnext (first aseq)))
        (when (next aseq)
          (.write ^java.io.Writer *out* ", ")
          (when (and (complex? (next aseq)) (complex? (fnext aseq)) (complex? (second (fnext aseq))))
            (pprint-newline :linear))
          (recur (next aseq))))))))

(defn- pprint-map-sorted [amap]
  (try
    (pprint-map (into (sorted-map) amap))
    (catch ClassCastException e (pprint-map amap))))

(defmulti color-dispatch class)

;; (use-method simple-dispatch clojure.lang.IPersistentSet pprint-set)
;; (use-method simple-dispatch clojure.lang.PersistentQueue pprint-pqueue)
;; (use-method simple-dispatch clojure.lang.IDeref pprint-ideref)

(use-method color-dispatch clojure.lang.IPersistentMap pprint-map-sorted)
(use-method color-dispatch clojure.lang.IPersistentVector pprint-vector)
(use-method color-dispatch clojure.lang.Keyword (partial raw-color-pprint :green))
(use-method color-dispatch java.lang.Long (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.Double (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.String (partial color-pprint :yellow))
(use-method color-dispatch nil pr)
(use-method color-dispatch :default pprint/simple-dispatch)

