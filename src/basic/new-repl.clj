(ns basic.new-repl
  (:require [basic.util :refer [dissoc-values-where]]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [instaparse.core :as ip])
  (:import (clojure.lang PersistentQueue)))

(defn error? [cxt]
  (contains? cxt :error))

(def rudiments
  (ip/parser
   "basic = (string | quit)*
    quit = <'QUIT'>
    <string> = <'\"'> #'[^\"]' <'\"'>"
    <ws> = \s+
   :string-ci true))

(defn perform-output! [cxt handler]
  (if (empty? (:output cxt))
    cxt
    (recur (handler cxt) handler)))

(defn perform-input! [cxt handler!]
  (if (:input-blocked? cxt)
    (handler! cxt)
    cxt))

(defn perform-io-or-error! [cxt in-handler out-handler err-handler]
  (if (error? cxt)
    (err-handler cxt)
    (-> cxt
        (perform-output! out-handler)
        (perform-input! in-handler))))

(defn show-output! [cxt]
  (print (peek (:output cxt)))
  (flush)
  (update-in cxt [:output] pop))

(defn get-input! [cxt]
  (update-in cxt [:input] #(conj % (edn/read-string (read-line))))
  cxt)

(defn hide-extraneous [cxt]
  (-> cxt
      (update-in [:symbols]
                 (fn [s] (dissoc-values-where s #(= (get % :kind)
                                                   :builtin))))
      (dissoc :program)
      (update-in [:ip] first)))

(defn dump-cxt [cxt]
  (pp/pprint (hide-extraneous cxt))
  cxt)

(defn handle-error! [cxt]
  (print "CRASH:" (:error cxt))
  (when (:exception cxt)
    (println "TRACE:" (.printStackTrace (:trace cxt))))
  (flush)
  (dump-cxt cxt))

(defn prompt [cxt]
  (print "> ")
  (flush)
  (get-input! cxt))

(defn interpret [cxt]
  )

(defn repl []
  (loop [cxt {:input PersistentQueue/EMPTY :output PersistentQueue/EMPTY}]
    ))
