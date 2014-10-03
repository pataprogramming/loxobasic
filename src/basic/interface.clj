(ns basic.interface
  (:require [basic.interpreter :refer [step fresh-context push-input]]
            [clojure.core.async :refer [chan go alt! alt!! >! <!!]]
            [basic.util :refer [dissoc-values-where]]
            [clojure.pprint :as pp]))

(defn make-interface
  ([] (make-interface (fresh-context)))
  ([cxt] {:state (atom cxt)
          :input (chan)
          :output (chan)
          :control (chan)}))

(defn stuff [iface st]
  (go (>! (:input iface) st)))

(defn drain [iface]
  (println (<!! (:output iface))))

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

(defn spin! [iface & [max-steps]]
  (loop [steps 0]
    (when (and (not (:iterminated? (:state iface)))
               (or (not max-steps) (< steps max-steps)))
      (println "loop start step" steps)
      (let [v         (go (alt!! (:control iface) :terminated
                                 :default         :continuing))
            ctrl-chan (:control iface)
            out-chan  (:output iface)
            in-chan   (:input iface)
            state     (:state iface)]
        (if (= (<!! v) :continuing)
          (do
            (println "continuing")
            (cond
             ;; Send all pending output to the output channel
             (not (empty? (get @state :output)))
             (do
               (println "outputing")
               (doseq [st (get @state :output)] (go (>! out-chan st)))
               (swap! state #(update-in % [:output] empty))
               (recur (inc steps)))

             ;; If input is needed, wait for it to be provided
             (get @state :input-blocked?)
             (do
               (println "inputting")
               (let [v (alt!!
                             in-chan   ([v] v)
                             ctrl-chan :terminacted)]
                 (when-not (= v :terminated)
                   (swap! state #(push-input % v))
                   (recur (inc steps)))))
             ;; Otherwise, execute the next program step
             :else
             (do
               (println "stepping")
               (swap! state step)
               (dump-cxt @state)
               (recur (inc steps)))))
          (println "terminating"))))))

(def i (make-interface))
