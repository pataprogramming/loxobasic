(ns basic.repl
  (:require [basic.interpreter :refer [action-reset execute run store
                                       store-program]]
            [basic.parser :refer [parse]]
            [clojure.pprint :as pp]
            [instaparse.core :as ip]))

;;;; Cheap console I/O for REPL testing

(defn show-output! [cxt]
  (print (peek (:output cxt)))
  (flush)
  (update-in cxt [:output] pop))

(defn get-input! [cxt]
  (update-in cxt [:input] #(conj % (clojure.edn/read-string (read-line))))
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


;;;; Basic runners for REPL testing

(defn dissoc-values-where [m pred]
                (reduce (fn [acc kv]
                          (if (pred (val kv))
                            (dissoc acc (key kv))
                            acc)) m m))

(defn handle-parsed [cxt ast]
  (case (first ast)
    :program (store-program (action-reset cxt) ast)
    :line    (store cxt ast)))


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
                        (show-output!)
                        (get-input!)))]
    ;;(dump-cxt next-cxt)
    (recur next-cxt)))

(defn srun [prog]
  (pp/pprint (->> prog
                  parse
                  (#(store-program (action-reset {}) %))
                  (#(run % get-input! show-output! handle-error!)))))

(defn -main [& args]
  (println "LoxoBASIC Interpreter")
  (repl (action-reset {})))
