(ns repl-color.core
  (:require [clojure.pprint :as pprint]
            [clansi.core :as clansi])
  (:import [java.io Writer]))

;; (def ^:dynamic *pprint-writer* *out*)

(defn- use-method
  "Installs a function as a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val func]
  (. multifn addMethod dispatch-val func))

(defn- color-pprint [color obj]
  (pr (clansi/style obj color)))

(defn- raw-color-pprint [color obj]
  (binding [*print-readably* false
            *print-dup* false]
    (color-pprint color obj)))

(defmulti color-dispatch class)

(use-method color-dispatch clojure.lang.Keyword (partial raw-color-pprint :green))
(use-method color-dispatch java.lang.Long (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.Double (partial raw-color-pprint :blue))
(use-method color-dispatch java.lang.String (partial color-pprint :yellow))
(use-method color-dispatch nil pr)
(use-method color-dispatch :default pprint/simple-dispatch)

(pprint/set-pprint-dispatch color-dispatch)

