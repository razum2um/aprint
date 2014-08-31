(ns repl-color.core
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [clansi.core :as clansi])
  (:import [java.io Writer]))

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


;; (defn- pprint-map [amap]
;;   (try
;;     (#'pprint/pprint-map (into (sorted-map) amap))
;;     (catch ClassCastException e (#'pprint/pprint-map amap))))

(defn- scalar? [x]
  (or (number? x) (string? x) (nil? x)))

(defn- complex? [x]
  (not (scalar? x)))

(use 'alex-and-georges.debug-repl)

(defn- pprint-map [amap]
  ;; (debug-repl)
  (let [color (first (shuffle [:red :blue :cyan]))]
  (pprint-logical-block :prefix "{" :suffix "}" ;;:per-line-prefix "!!!"
    (print-length-loop [aseq (seq amap)]
    ;; (loop [aseq (seq amap)]
      (when aseq
	      ;; (pprint-logical-block 
            (write-out (ffirst aseq))
            (.write ^java.io.Writer *out* " ")
          ;; (push-thread-bindings {
          ;;                        #'clojure.pprint/*current-level*
          ;;                           (dec (var-get #'clojure.pprint/*current-level*))
          ;;                           #'clojure.pprint/*current-length* 0})
            ;; (when complex? (second (first aseq))
            ;;   (pprint-newline :linear))
            (write-out (fnext (first aseq)))
          ;; )
        (when (next aseq)
          (.write ^java.io.Writer *out* ", ")
          (when (and (complex? (next aseq)) (complex? (fnext aseq)) (complex? (second (fnext aseq))))
            (pprint-newline :linear))
          (recur (next aseq)))
        
        ))
                        )
    ))

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

