(ns repl-color.core
  (:require [clojure.pprint :as pprint :refer [pprint-logical-block write-out pprint-newline print-length-loop]]
            [clansi.core :as clansi]
            [clojure.string :as s])
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

(defmacro ^{:private true} binding-map [amap & body]
  (let []
    `(do
       (. clojure.lang.Var (pushThreadBindings ~amap))
       (try
        ~@body
        (finally
         (. clojure.lang.Var (popThreadBindings)))))))

;; debug

(defmulti ^{:private true} tok :type-tag)
(defmethod tok :nl-t [token]
  (:type token))
(defmethod tok :buffer-blob [token]
  (str \" (:data token) (:trailing-white-space token) \"))
(defmethod tok :default [token]
  (:type-tag token))
(defn- toks [toks] (map tok toks))

(defn- prerr [& args]
  "Println to *err*"
  (binding [*out* *err*]
    (apply println args)))

(defmacro ^{:private true} prlabel [prefix arg & more-args]
  "Print args to *err* in name = value format"
  `(prerr ~@(cons (list 'quote prefix) (mapcat #(list (list 'quote %) "=" %) 
                                                  (cons arg (seq more-args))))))
;; end debug

(defstruct ^{:private true} logical-block
           :parent :section :start-col :indent
           :done-nl :intra-block-nl
           :prefix :per-line-prefix :suffix
           :logical-block-callback)

(defn- emit-nl [^Writer this nl]
  (.write (getf :base) (#'pprint/pp-newline))
  (dosync (setf :trailing-white-space nil))
  (let [lb (:logical-block nl)
        ^String prefix (:per-line-prefix lb)] 
    (if prefix 
      (.write (getf :base) prefix))
    (let [^String istr (apply str (repeat (- @(:indent lb) (count prefix))
					  \space))] 
      (.write (getf :base) istr))
    (#'pprint/update-nl-state lb)))

(defn- write-token-string [this tokens]
  (let [[a b] (#'pprint/split-at-newline tokens)]
   ;; (prlabel wts (toks a) (toks b))
    (if a (#'pprint/write-tokens this a false))
    (if b
      (let [[section remainder] (#'pprint/get-section b)
            newl (first b)]
;;         (prlabel wts (toks section)) (prlabel wts (:type newl)) (prlabel wts (toks remainder)) 
        (let [do-nl (emit-nl newl this section (#'pprint/get-sub-section b))
              result (if do-nl 
                       (do
;;                          (prlabel emit-nl (:type newl))
                         (emit-nl this newl)
                         (next b))
                       b)
              long-section (not (#'pprint/tokens-fit? this result))
              result (if long-section
                       (let [rem2 (write-token-string this section)]
;;;                              (prlabel recurse (toks rem2))
                         (if (= rem2 section)
                           (do ; If that didn't produce any output, it has no nls
                                        ; so we'll force it
                             (#'pprint/write-tokens this section false)
                             remainder)
                           (into [] (concat rem2 remainder))))
                       result)
;;              ff (prlabel wts (toks result))
              ] 
          result)))))

(defn- write-line [^Writer this]
  (dosync
   (loop [buffer (getf :buffer)]
;;     (prlabel wl1 (toks buffer))
     (setf :buffer (into [] buffer))
     (if (not (#'pprint/tokens-fit? this buffer))
       (let [new-buffer (write-token-string this buffer)]
;;          (prlabel wl new-buffer)
         (if-not (identical? buffer new-buffer)
                 (recur new-buffer)))))))

(defn- add-to-buffer [^Writer this token]
;  (prlabel a2b token)
  (dosync
   (setf :buffer (conj (getf :buffer) token))
   (if (not (#'pprint/tokens-fit? this (getf :buffer)))
     (write-line this))))

;; printer

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
                 (let [
                       ;; ansi-codes (vals clansi.core/ANSI-CODES)
                       ;; str-size (count s)
                       ;; plain-str (reduce #(s/replace %1 %2 "") s ansi-codes)
                       ;; plain-str-size (count plain-str)
                       ;; offset (- plain-str-size str-size)
                       oldpos (getf :pos)
                       newpos (+ oldpos (count s0))
                       ;; newpos (+ oldpos (count s0) -8)
                       ;; newpos (+ oldpos (count s0) offset)
                       ]
                   (setf :pos newpos)
                   ;; (.write *err* (str "S: " s " | OFFSET:" newpos "\n"))
                   ;; (.flush *err*)
                   (add-to-buffer this (#'pprint/make-buffer-blob s white-space oldpos newpos))))))

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
         (binding-map (if (or (not (= pprint/*print-base* 10)) pprint/*print-radix*) {#'pr #'pprint/pr-with-base} {}) 
           (write-out object)))
       (if (not (= 0 (#'pprint/get-column *out*)))
         (prn)))))
