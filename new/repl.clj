(ns basic.repl
  (:require [basic.util :refer [dissoc-values-where]]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [instaparse.core :as ip])
  (:import (clojure.lang PersistentQueue)))

(defn error? [cxt]
  (contains? cxt :error))

(def basic
  (ip/parser
   "basic = <ws*> entry? <nl?> (<nl> entry <nl?>)*
    <entry> = (string | quit)
    quit = <'QUIT'>
    string = <'\"'> #'[^\"]*' <'\"'>
    <ws> = #'[ \\\\t\\\\x0B\\f]+'
    <nl> = #'\\s*[\\n\\r]\\s*'
    "
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
  (update-in cxt [:input] #(conj % (edn/read-string (read-line)))))

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

(defn action-print [cxt & [args]]
  (-> cxt
      (update-in [:output] #(conj (peek (:output (str args)))))))

(defn action-shutdown [cxt]
  (assoc-in cxt [:shutdown?] true))

(defn interpret [cxt]
  (when-let [input (seq (:input cxt))]
    (-> cxt
        (update-in [:output] pop))
    (case (first input)
      :string (action-print (rest input)))))

(defn repl []
  (loop [cxt {:input PersistentQueue/EMPTY
              :output PersistentQueue/EMPTY
              :shutdown? false}]
    (let [cxt (-> cxt
                  (perform-output! show-output!)
                  prompt
                  (perform-input! get-input!)
                  interpret)]
      (when-not (:shutdown? cxt)
        (recur cxt)))))
