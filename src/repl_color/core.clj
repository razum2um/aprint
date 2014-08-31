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


(import [clojure.lang IDeref]
        [java.io Writer])

(definterface PrettyFlush
  (^void ppflush []))

(defmacro ^{:private true} 
  getf 
  [sym]
  `(~sym @@~'this))

(defmacro ^{:private true} 
  setf [sym new-val] 
  `(alter @~'this assoc ~sym ~new-val))

(defstruct ^{:private true} logical-block
           :parent :section :start-col :indent
           :done-nl :intra-block-nl
           :prefix :per-line-prefix :suffix
           :logical-block-callback)

(defn- pretty-writer [writer max-columns miser-width]
  (let [lb (struct logical-block nil nil (ref 0) (ref 0) (ref false) (ref false))
        fields (ref {:pretty-writer true
                     :base (#'pprint/column-writer writer max-columns)
                     :logical-blocks lb 
                     :sections nil
                     :mode :writing
                     :buffer []
                     :buffer-block lb
                     :buffer-level 1
                     :miser-width miser-width
                     :trailing-white-space nil
                     :pos 0})]
    (proxy [Writer IDeref PrettyFlush] []
      (deref [] fields)

      (write 
       ([x]
          ;;     (prlabel write x (getf :mode))
          (condp = (class x)
            String 
            (let [^String s0 (#'pprint/write-initial-lines this x)
                  ^String s (.replaceFirst s0 "\\s+$" "")
                  white-space (.substring s0 (count s))
                  mode (getf :mode)]
              (dosync
               (if (= mode :writing)
                 (do
                   (#'pprint/write-white-space this)
                   (.write (getf :base) s)
                   (setf :trailing-white-space white-space))
                 (let [oldpos (getf :pos)
                       newpos (+ oldpos (count s0))]
                   (setf :pos newpos)
                   ;; (.write *err* (str "COUNT" (count s)))
                   ;; (.flush *err*)
                   (#'pprint/add-to-buffer this (#'pprint/make-buffer-blob s white-space oldpos newpos))))))

            Integer
            (#'pprint/p-write-char this x)
            Long
            (#'pprint/p-write-char this x))))

      (ppflush []
             (if (= (getf :mode) :buffering)
               (dosync
                (#'pprint/write-tokens this (getf :buffer) true)
                (setf :buffer []))
               (#'pprint/write-white-space this)))

      (flush []
             (.ppflush this)
             (.flush (getf :base)))

      (close []
             (.flush this)))))

(defn- make-pretty-writer
  [base-writer right-margin miser-width]
  (pretty-writer base-writer right-margin miser-width))

(defmacro ^{:private true} with-pretty-writer [base-writer & body]
  `(let [base-writer# ~base-writer
         new-writer# (not (#'pprint/pretty-writer? base-writer#))]
     (binding [*out* (if new-writer#
                      (make-pretty-writer base-writer# pprint/*print-right-margin* pprint/*print-miser-width*)
                      base-writer#)]
       ~@body
       (.ppflush *out*))))

(defn ppprint 
  ([object] (ppprint object *out*)) 
  ([object writer]
     (with-pretty-writer writer
       (binding [pprint/*print-pretty* true]
         (#'pprint/binding-map (if (or (not (= pprint/*print-base* 10)) pprint/*print-radix*) {#'pr #'pprint/pr-with-base} {}) 
           (write-out object)))
       (if (not (= 0 (#'pprint/get-column *out*)))
         (prn)))))
