(ns basic.util)

(defmacro def- [item value]
  `(def ^{:private true} ~item ~value))

(defn compare-seq [[a & as] [b & bs]]
  (if (and (nil? a) (nil? b))
    0
    (case (compare a b)
      -1 -1
      1  1
      0  (recur as bs))))

(defn dissoc-values-where [m pred]
                (reduce (fn [acc kv]
                          (if (pred (val kv))
                            (dissoc acc (key kv))
                            acc)) m m))
