(ns basic.interface
  (:require [basic.interpreter :refer [input-or-step]]
            [clojure.core.async :refer [chan go alt! >! <!!]]))

(defn make-interface [cxt]
  {:state (atom cxt)
   :input (chan)
   :output (chan)
   :control (chan)})

(defn spin! [iface]
  (loop []
    (let [[v ch]    (go (alt! (:control iface) :terminated
                              :default         :continuing))
          ctrl-chan (:control iface)
          out-chan  (:output iface)
          in-chan   (:input iface)
          state     (:state iface)]
      (when (= v :continuing)
        (cond
         ;; Send all pending output to the output channel
         (get @state :output)
         (do
           (doseq [st (get @state :output)] (go (>! out-chan st)))
           (swap! state #(update-in % [:output] empty))
           (recur))

         ;; If input is needed, wait for it to be provided
         (get state :input-blocked?)
         (let [v (go (alt!
                       ctrl-chan :terminated
                       in-chan   ([v ch] v)))]
           (when-not (= v :terminated)
             (swap! state #(update-in % [:input]))))

         ;; Otherwise, execute the next program step
         :else
         (do
           (swap! state #(input-or-step))
           (recur)))))))
