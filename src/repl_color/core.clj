(ns repl-color.core
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [clansi.core :as clansi])
  (:import [java.io Writer]))

;; (def ^:dynamic *pprint-writer* *out*)

;; (defmethod print-method clojure.lang.IPersistentVector [v, ^Writer w]
;;   (#'clojure.core/print-meta v w)
;;   (#'clojure.core/print-sequential (clansi/style "[" :magenta) #'clojure.core/pr-on " " (clansi/style "]" :magenta) v w))
;;
;; (defmethod print-method java.util.RandomAccess [v, ^Writer w]
;;   (if *print-readably*
;;     (do
;;       (#'clojure.core/print-meta v w)
;;       (#'clojure.core/print-sequential (clansi/style "[" :magenta) #'clojure.core/pr-on " " (clansi/style "]" :magenta) v w))
;;     (#'clojure.core/print-object v w)))

(def use-method #'pprint/use-method)

(defn- color-pprint [color obj]
  (pr (clansi/style obj color)))

(defn- raw-color-pprint [color obj]
  (binding [*print-readably* false
            *print-dup* false]
    (color-pprint color obj)))

(defn- pprint-vector [avec]
  (pprint-logical-block :prefix (clansi/style "[" :magenta) :suffix (clansi/style "]" :magenta)
                               (print-length-loop [aseq (seq avec)]
                                                         (when aseq
                                                           (write-out (first aseq))
                                                           (when (next aseq)
                                                             (.write ^java.io.Writer *out* " ")
                                                             (pprint-newline :linear)
                                                             (recur (next aseq)))))))

                                                      ;; (def x #'pprint/*current-length*)
(defn- pprint-map [amap]
  (let [color (shuffle [:cyan :red :blue])]
    (pprint-logical-block :prefix (clansi/style "{" color) :suffix (clansi/style "}" color)
                          (print-length-loop [aseq (seq amap)]
                                             (when aseq
                                               (pprint-logical-block 
                                                 (write-out (ffirst aseq))
                                                 (.write ^java.io.Writer *out* " ")
                                                 (pprint-newline :linear)
                                                 ;; (set! [pprint/*current-length* 0]
                                                 (write-out (fnext (first aseq))))
                                               (when (next aseq)
                                                 (.write ^java.io.Writer *out* ", ")
                                                 (pprint-newline :linear)
                                                 (recur (next aseq))))))))

(defmulti color-dispatch class)

;; (use-method simple-dispatch clojure.lang.IPersistentSet pprint-set)
;; (use-method simple-dispatch clojure.lang.PersistentQueue pprint-pqueue)
;; (use-method simple-dispatch clojure.lang.IDeref pprint-ideref)

(use-method color-dispatch clojure.lang.IPersistentMap pprint-map)
(use-method color-dispatch clojure.lang.IPersistentVector pprint-vector)
(use-method color-dispatch clojure.lang.Keyword (partial raw-color-pprint :green))
(use-method color-dispatch java.lang.Long (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.Double (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.String (partial color-pprint :yellow))
(use-method color-dispatch nil pr)
(use-method color-dispatch :default pprint/simple-dispatch)

(pprint/set-pprint-dispatch color-dispatch)

