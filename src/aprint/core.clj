(ns aprint.core
  (:require [clojure.pprint :as pprint :refer [write-out with-pprint-dispatch]]
            [aprint.utils :refer :all]
            [aprint.tty :refer [tty-width clear-screen]]
            [aprint.dispatch :refer [color-dispatch]]
            [aprint.writer :refer [with-awesome-writer]]))

(def ^:dynamic *use-aprint* false)

(defn aprint
  ([object] (aprint object *out*))
  ([object writer]
   (if (need-clear-screen? object)
     (clear-screen))
   (binding [pprint/*print-right-margin* (tty-width)]
     (with-pprint-dispatch color-dispatch
       (with-awesome-writer writer
         (binding [pprint/*print-pretty* true
                   ]
           (binding-map (if (or (not (= pprint/*print-base* 10)) pprint/*print-radix*) {#'pr #'pprint/pr-with-base} {}) 
                        (write-out object)))
         (if (not (= 0 (#'pprint/get-column *out*)))
           (prn)))))))

(defmacro ap []
  `(aprint *1))

