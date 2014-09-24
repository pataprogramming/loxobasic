(ns basic.interface
  (:require [basic.interpreter :refer [input-or-step]]
            [core.async :refer [chan go >! <!!]]))

(defn make-interface [cxt]
  {:state (atom cxt)
   :input (chan)
   :output (chan)
   :control (chan)})

(defn spin! [iface]
  (loop []
    (cond
     ;; FIXME: Add case to check :control chan (break stepping)

     ;; Send all pending output to the output channel
     (get (:state @iface) :output)
     (do
       (doseq [st out-strings] (go (>! (:output iface) st)))
       (swap! (:state iface) #(update-in % [:output] empty))
       (recur))

     ;; If input is needed, wait for it to be provided
     (get (:state @iface) :input-blocked?)
     (let [[v ch] (go (alt!
                       (:control iface) iface
                       (:input iface)))]
      (when (= ch (:input iface))
        (swap! (:state iface) #(update-in % [:input ]))))

     ;; Otherwise, execute the next program step
     :else
     (do
       (swap! (:state iface) #(input-or-step)))
       (recur))))
