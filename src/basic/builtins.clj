(ns basic.builtins
  (:require [basic.parser :refer [process-number-string]]
            [basic.util :refer [def-]]))

(defn- make-builtin [cxt [name params function]]
  (assoc-in cxt [:symbols [:id name]]
            {:kind     :builtin
             :params   (vec (map #(vector :id %) params))
             :function function}))

(def- builtins
  [["ABS"    ["X"]          (fn builtin-abs    [x]      (Math/abs x))]
   ["ASC"    ["X$"]         (fn builtin-asc    [x]      (int (.charAt x 0)))]
   ["ATN"    ["X"]          (fn builtin-atn    [x]      (Math/atan x))]
   ["CHR$"   ["X"]          (fn builtin-chr$   [x$]     (str (char x$)))]
   ["COS"    ["X"]          (fn builtin-cos    [x]      (Math/cos x))]
   ["EXP"    ["X"]          (fn builtin-exp    [x]      (Math/exp x))]
   ["INT"    ["X"]          (fn builtin-int    [x]      (int x))]
   ["LEFT$"  ["X$","Y"]     (fn builtin-left$  [x$,y]   (subs x$ 0 y))]
   ["LEN"    ["X$"]         (fn builtin-len    [x$]     (count x$))]
   ["LOG"    ["X"]          (fn builtin-log    [x]      (Math/log x))]
   ["MID$"   ["X$","Y","Z"] (fn builtin-mid$   [x$,y,z] (subs x$ (dec y) (+ (dec y) z)))]
   ["RND"    ["X"]          (fn builtin-rnd    [x]      (* x (rand)))]
   ["RIGHT$" ["X$","Y"]     (fn builtin-right$ [x$,y]   (subs x$ (- (count x$) y)))]
   ["SGN"    ["X"]          (fn builtin-sgn    [x]      (int (Math/signum (double x))))]
   ["SIN"    ["X"]          (fn builtin-sin    [x]      (Math/sin x))]
   ["SQR"    ["X"]          (fn builtin-sqr    [x]      (Math/sqrt x))]
   ["STR$"   ["X"]          (fn builtin-str    [x]      (str x))]
   ["TAB"    ["X"]          (fn builtin-tab    [x]      (apply str (repeat x " ")))]
   ["TAN"    ["X"]          (fn builtin-tan    [x]      (Math/tan x))]
   ["VAL"    ["X$"]         (fn builtin-val    [x$]     (process-number-string x$))]])

(defn generate-builtins [cxt]
  (reduce make-builtin cxt builtins))
