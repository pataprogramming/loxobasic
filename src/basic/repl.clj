(ns basic.repl
  (:require [basic.interpreter :refer [action-reset execute
                                       maybe-advance-ip
                                       reset-data-pointer set-error
                                       store store-program]]
            [basic.parser :refer [parse]]
            [clojure.pprint :as pp]
            [instaparse.core :as ip]))

;;;; Cheap console I/O for REPL testing

(defn show-output [cxt]
  (if (empty? (:output cxt))
    cxt
    (do
      (print (peek (:output cxt)))
      (flush)
      (recur (update-in cxt [:output] pop)))))

(defn get-input [cxt]
  (if (:input-blocked? cxt)
    (update-in cxt [:input] #(conj % (clojure.edn/read-string (read-line))))
    cxt))


;;;; Basic runners for REPL testing

(defn dissoc-values-where [m pred]
                (reduce (fn [acc kv]
                          (if (pred (val kv))
                            (dissoc acc (key kv))
                            acc)) m m))

(defn hide-extraneous [cxt]
  (-> cxt
      (update-in [:symbols]
                 (fn [s] (dissoc-values-where s #(= (get % :kind) :builtin))))
      (dissoc :program)
      (update-in [:ip] first)))

(defn dump-cxt [cxt]
  (pp/pprint (hide-extraneous cxt))
  cxt)

(defn clear-error [cxt]
  (dissoc cxt :error :trace))

(defn error? [cxt]
  (contains? cxt :error))

(defn initialize [cxt]
  (-> cxt
      (assoc :ip (:program cxt))
      (assoc :data-pointer [nil nil])
      (assoc :running? true)
      (assoc :jumped? false)
      (assoc :substack '())
      (assoc :for-map {})
      (assoc :output (clojure.lang.PersistentQueue/EMPTY))
      (assoc :input (clojure.lang.PersistentQueue/EMPTY))
      (assoc :input-blocked? false)
      (assoc :echo-input? true)
      (clear-error)
      (reset-data-pointer)))

(defn step [cxt]
  (let [stmt (val (first (:ip cxt)))]
    (try
      (execute cxt stmt)
      (catch Exception e
        (set-error cxt "Clojure host error" e)))))

(defn perform-io-or-error [cxt]
  (if (error? cxt)
    (do (print "CRASH:" (:error cxt))
        (when (:exception cxt)
          (println "TRACE:" (.printStackTrace (:trace cxt))))
        (flush)
        (dump-cxt cxt))
    (-> cxt
        (show-output)
        (get-input))))

(defn run [cxt]
  (loop [cxt (initialize cxt)]
    (let [cxt (-> cxt
                  (step)
                  (perform-io-or-error)
                  (maybe-advance-ip))]
      (if (and (:running? cxt) (:ip cxt))
        (recur cxt)
        (-> cxt (dissoc :ip :running? :jumped? :substack))))))

(defn repl [cxt]
  (print "> ")
  (flush)
  (let [input    (read-line)
        parsed   (try (parse input)
                      (catch Exception e
                        (str "PARSE ERROR:" (.getMessage e))
                        (.printStackTrace e)))
        ;;_        (println "TRYING TO INTERPRET" parsed)
        next-cxt  (if (ip/failure? parsed)
                    (println "PARSE FAILED:" parsed)
                    (-> cxt
                        (#(case (first parsed)
                            :program   (store-program % parsed)
                            :directive (execute % (fnext parsed))
                            (do (println parsed) %)))
                        (show-output)
                        (get-input)))]
    ;;(dump-cxt next-cxt)
    (recur next-cxt)))

(defn handle-parsed [cxt ast]
  (case (first ast)
    :program (store-program (action-reset cxt) ast)
    :line    (store cxt ast)))

(defn srun [prog]
  (pp/pprint (->> prog
                  parse
                  (#(store-program (action-reset {}) %))
                  run)))

(defn -main [& args]
  (println "LoxoBASIC Interpreter")
  (repl (action-reset {})))
