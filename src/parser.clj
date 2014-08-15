(ns basic.parser
  (:require [instaparse.core :as ip]
            [clojure.edn]
            [clojure.data.avl :as avl]))

(def tt "10 A=1\n20 B=2\n10 C=3\n30 D=4\n50 D=5")

(defn compare-pair [[a b] [c d]]
  (case (compare a c)
    -1 -1
    1  1
    0  (compare b d)))

(def basic
  (ip/parser
   "program       = lines
                  | <ws*> statement <ws*> <nl*>
    <lines>         = lines <nl+> line
                  | line <nl*>
    line          = <ws*> label <ws> statement <ws*>
    label         = integer
    statement     = assignment
                  | print

    print         = <'PRINT' <ws>> expression

    assignment    = <('LET' <ws>)?> id <ws*> <'='> <ws*> expression

    constant      = integer | string
    expression    = and_exp <ws*> 'OR' <ws*> expression
                  | and_exp
    and_exp     = not_exp <ws*> 'AND' <ws*> and_exp
                  | not_exp
    not_exp     = 'NOT' <ws*> compare_exp
                  | compare_exp
    compare_exp = add_exp <ws*> '='  <ws*> compare_exp
                  | add_exp <ws*> '<>' <ws*> compare_exp
                  | add_exp <ws*> '><' <ws*> compare_exp
                  | add_exp <ws*> '>'  <ws*> compare_exp
                  | add_exp <ws*> '>=' <ws*> compare_exp
                  | add_exp <ws*> '<'  <ws*> compare_exp
                  | add_exp <ws*> '<=' <ws*> compare_exp
                  | add_exp
    add_exp     = mult_exp <ws*> '+' <ws*> add_exp
                  | mult_exp <ws*> '-' <ws*> add_exp
                  | mult_exp
    mult_exp    = negate_exp <ws*> '*' <ws*> mult_exp
                  | negate_exp <ws*> '/' <ws*> mult_exp
                  | negate_exp
    negate_exp  = '-' <ws*> power_exp
                  | power_exp
    power_exp   = power_exp <ws*> '^' <ws*> value
                  | value
    <value>       = <'('> expression <')'>
                  | id
                  | constant

    integer = digit+
    <digit> = #'[0-9]'
    <string> = <'\"'> #'[^\"]*' <'\"'>
    <alpha> = #'[A-Za-z]'
    alphanum = (alpha | digit)+
    id       = alpha alphanum*
    nl = #'[\n\r]+'
    ws = #'[ \t\f]+'"))

(defn treeify
  ([a]
     a)
  ([a b]
     [(keyword a) b])
  ([a b c]
     [(keyword b) a c]))

(defn process [s]
  (ip/transform
   {:integer     (comp clojure.edn/read-string str)
    :alphanum    str
    :id          (fn idify [& id] [:id (apply str id)])
    :expression  treeify
    :and_exp     treeify
    :not_exp     treeify
    :compare_exp treeify
    :add_exp     treeify
    :mult_exp    treeify
    :negate_exp  treeify
    :power_exp   treeify
    ;;:assignment  (fn ass-to-stmt [& ass] (apply vector "LET" ass))
    :statement   (fn stmtify [[action & args]] {:action action :args (vec args)})
    :line        (fn lineify [[_ label] statement] (assoc statement :label label))
    :program     (fn programmify [& lines]
                   (reduce (fn [acc {:keys [label] :as line}] (assoc acc label line))
                           (avl/sorted-map)
                           lines))
    }
   s))

(declare express)

(defn bbool [cxt exp]
  (let [v (express cxt exp)]
    (case v
      true  -1
      false  0
      0      0
      -1)))

(defn bfalse? [cxt exp]
  (let [v (bbool (express cxt exp))]
    (= 0 v)))

(defn btrue? [cxt exp]
  (not (bfalse? exp)))

(defn express [cxt exp]
  (if (coll? exp)
    (let [[typ a b] exp
          ;a (express cxt (fnext exp))
          ;b (express cxt (first (next (next exp))))
          ]
      (println "type:" typ "a:" a "b:" b)
      (case typ
        nil       nil
        :constant a
        :id       (get-in cxt [:vars a])
        :+        (+ (express cxt a) (express cxt b))
        :-        (if (not (nil? (express cxt b)))
                    (- (express cxt a) (express cxt b))
                    (- (express cxt a)))
        :*        (* (express cxt a) (express cxt b))
        :/        (/ (express cxt a) (express cxt) b)
        :AND      (bbool cxt (and (btrue? cxt a) (btrue? cxt b)))
        :OR       (bbool cxt (or  (btrue? cxt a) (btrue? cxt b)))
        :NOT      (bbool cxt (not (btrue? cxt a)))
        :=        (bbool cxt (= (express cxt a) (express cxt b)))
        :>        (bbool cxt (> (express cxt a) (express cxt b)))
        :<        (bbool cxt (< (express cxt a) (express cxt b)))
        :<=       (bbool cxt (<= (express cxt a) (express cxt b)))
        :>=       (bbool cxt (>= (express cxt a) (express cxt b)))
        :<>       (bbool cxt (not= (express cxt a) (express cxt b)))
        :><       (bbool cxt (not= (express cxt a) (express cxt b)))
        (throw (Throwable. (str "can't handle" typ)))
        ))
    exp))

(defn store [cxt line]
  (assoc-in cxt [:program (:label line)] line))

(defn store-program [cxt program]
  (assoc cxt :program program))



(defn execute [cxt {:keys [action args] :as stmt}]
  (println "trying to execute " stmt)
  (println "action:" action "args:" args)
  (case action
    :assignment
    (assoc-in cxt [:vars (second (first args))] (express cxt (fnext args)))
    :print (do
              (println "OUTPUT:" (apply express cxt args))
              cxt)))

(defn interpret [cxt line]
  (let [ast (first (vals  (process (basic line))))]
    (println "ast:" ast)
    (if (contains? ast :label)
      (store cxt ast)
      (execute cxt ast))))

(defn maybe-advance [cxt]
  (if (:jumped? cxt)
    cxt
    (update-in cxt [:ip] next)))

(defn run [cxt]
  (loop [cxt  (-> cxt
                  (assoc :ip (:program cxt))
                  (assoc :running? true)
                  (assoc :jumped? false))]
    (let [stmt (val (first (:ip cxt)))
          cxt  (->
                (execute cxt stmt)
                (maybe-advance))]
      (if (:ip cxt)
        (recur cxt)
        (-> cxt (dissoc :ip :running? :jumped?))))))
