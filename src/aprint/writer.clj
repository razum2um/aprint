(ns aprint.writer
  (:require [clojure.pprint :as pprint]
            [clansi.core :as clansi]
            [clojure.string :as s])
  (:import [clojure.lang IDeref]
           [java.io Writer]))

(definterface PrettyFlush
  (^void ppflush []))

(defmacro ^{:private true}
  getf
  [sym]
  `(~sym @@~'this))

(defmacro ^{:private true}
  setf [sym new-val]
  `(alter @~'this assoc ~sym ~new-val))

(defn- get-field [^Writer this sym]
  (sym @@this))

(defn- set-field [^Writer this sym new-val] 
  (alter @this assoc sym new-val))

(defstruct ^{:private true} logical-block
           :parent :section :start-col :indent
           :done-nl :intra-block-nl
           :prefix :per-line-prefix :suffix
           :logical-block-callback)

;; "\e[\d+m"
(def ansi-code-pattern
  (re-pattern (reduce str (map char [27 92 91 92 100 43 109]))))

;; printer

(def ^:dynamic ^{:private true} *default-page-width* 72)

(defn- column-writer
  ([writer] (column-writer writer *default-page-width*))
  ([writer max-columns]
     (let [fields (ref {:max max-columns, :cur 0, :line 0 :base writer})]
       (proxy [Writer IDeref] []
         (deref [] fields)
         (flush []
           (.flush writer))
         (write
          ([^chars cbuf ^Integer off ^Integer len] 
             (let [^Writer writer (get-field this :base)] 
               (.write writer cbuf off len)))
          ([x]
             (condp = (class x)
               String 
               (let [^String s x
                     nl (.lastIndexOf s (int \newline))
                     plain-str (s/replace s ansi-code-pattern "")
                     ;; _ (prerr "!!! wow: nl:" nl)
                     ]
                 (dosync (if (neg? nl)
                           (set-field this :cur (+ (get-field this :cur) (count plain-str)))
                           (do
                             (set-field this :cur (- (count plain-str) nl 1))
                             (set-field this :line (+ (get-field this :line)
                                                      (count (filter #(= % \newline) plain-str)))))))
                 (.write ^Writer (get-field this :base) s))

               Integer
               (#'pprint/c-write-char this x)
               Long
               (#'pprint/c-write-char this x))))))))


(defn awesome-writer [writer max-columns miser-width]
  (let [lb (struct logical-block nil nil (ref 0) (ref 0) (ref false) (ref false))
        fields (ref {:pretty-writer true
                     :base (column-writer writer max-columns)
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
              ;; (prlabel write x (getf :mode))
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
                       str-size (count s)
                       ;; plain-str (reduce #(s/replace %1 %2 "") s ansi-codes)
                       plain-str (s/replace s ansi-code-pattern "")
                       plain-str-size (count plain-str)
                       offset (- plain-str-size str-size)
                       oldpos (getf :pos)
                       newpos (+ oldpos (count s0))
                       ;; newpos (+ oldpos (count s0) -8)
                       newpos (+ oldpos (count s0) offset)
                       ;; _ (prerr "!!! plain: " plain-str-size "bytes: " (into [] (.getBytes plain-str)))
                       ]
                   (setf :pos newpos)
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

(defn make-awesome-writer
  [base-writer right-margin miser-width]
  (awesome-writer base-writer right-margin miser-width))

(defmacro with-awesome-writer [base-writer & body]
  `(let [base-writer# ~base-writer
         new-writer# (not (#'pprint/pretty-writer? base-writer#))]
     (binding [*out* (if new-writer#
                      (make-awesome-writer base-writer# pprint/*print-right-margin* pprint/*print-miser-width*)
                      base-writer#)]
       ~@body
       (.ppflush *out*))))

