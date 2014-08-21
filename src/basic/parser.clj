(ns basic.parser
  (:require [instaparse.core :as ip]
            [clojure.edn]
            [clojure.data.avl :as avl]
            [clojure.walk :as w]
            [clojure.pprint :as pp]))

(def tt "10 A=1
         20 B=2
         30 C=3
         34 GOTO 40
         35 C=4
         40 D=4
         50 D=5
         60 GOSUB 100
         99 END
         100 E=5
         110 RETURN")

(def uu "10 J=2*3
         20 FOR I=0 TO J STEP 2
         30 PRINT I
         35 IF I=4 THEN STOP
         40 NEXT I")

(def vv "10  A=2:B=1
         20  ON A GOSUB 100,110,120
         30  ON B GOTO 50,60,70
         50  PRINT \"A\":STOP
         60  PRINT \"B\":STOP
         70  PRINT \"C\":STOP
         100 B=1:RETURN
         110 B=2:RETURN
         120 B=3:RETURN")

(defn compare-pair [[a b] [c d]]
  (case (compare a c)
    -1 -1
    1  1
    0  (compare b d)))

(defn compare-seq [[a & as] [b & bs]]
  (if (and (nil? a) (nil? b))
    0
    (case (compare a b)
      -1 -1
      1  1
      0  (recur as bs))))

(def basic
  (ip/parser
   "program       = lines
                  | <ws*> statements <ws*> <nl*>
    <lines>         = lines <nl+> line
                  | line <nl*>
    line          = <ws*> label <ws*> statements <ws*>
    label         = integer
    statements    = statement-list
    <statement-list>  = statement-list <ws* ':' ws*> statement
                  | statement
    statement     = assignment
                  | print
                  | remark
                  | if-then-else
                  | if-then
                  | for
                  | next
                  | goto
                  | gosub
                  | return
                  | on-goto
                  | on-gosub
                  | end

    print         = <'PRINT' <ws>> expression
    <notnl>       =#'[^\n\r]+'
    remark        = <'REM' notnl*>
    then-clause   = <'THEN' <ws>> (cond-destination | statements)
    cond-destination = expression
    else-clause   = <'ELSE' <ws>> (cond-destination | statements)

    if-then       = <'IF' <ws>> expression <ws> then-clause
    if-then-else  = <'IF' <ws>> expression <ws> then-clause <ws> else-clause

    for           = <'FOR' ws> id <ws*> <'=' ws*> expression <ws 'TO' ws> expression
                    (<ws 'STEP' ws> expression)?
    next          = <'NEXT' ws> id

    goto          = <'GOTO' <ws>> expression
    gosub         = <'GOSUB' <ws>> expression
    return        = <'RETURN'>
    end           = <'END' | 'STOP'>

    on-goto       = <'ON' ws> expression <ws 'GOTO' ws> expression-list
    on-gosub       = <'ON' ws> expression <ws 'GOSUB' ws> expression-list

    assignment    = <('LET' <ws>)?> id <ws*> <'='> <ws*> expression

    constant      = integer | string
    expression    = and_exp <ws*> 'OR' <ws*> expression
                  | and_exp

    expression-list = expression-list <ws* ',' ws*> expression
                    | expression
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

    integer = #'[0-9]+'
    <digit> = #'[0-9]'
    <string> = <'\"'> #'[^\"]*' <'\"'>
    <alpha> = #'[A-Za-z]'
    alphanum = #'[A-Za-z0-9]'
    id       = #'[A-Za-z][A-Za-z0-9$]*'
    nl = #'[\n\r]+'
    ws = #'[ \t\f]+'"))

(defn treeify
  ([a]
     a)
  ([a b]
     [(keyword a) b])
  ([a b c]
     [(keyword b) a c]))

(defn process-expressions [parse-tree]
  (ip/transform
   {:integer     (comp clojure.edn/read-string str)
    :alphanum    str
    :id          (fn idify [& id] [:id (apply str id)])
    :expression  #(vector :expression (treeify %))
    :and_exp     treeify
    :not_exp     treeify
    :compare_exp treeify
    :add_exp     treeify
    :mult_exp    treeify
    :negate_exp  treeify
    :power_exp   treeify}
   parse-tree))


(defn rewrite-cond-destination [t]
  (if (and (vector? t)
           (= (first t) :cond-destination))
    [:statements [:statement [:goto (fnext t)]]]
    t))

(defn append-to-labels [label t]
  (if (and (vector? t)
           (= (first t) :label))
    (conj t label)
    t))

(defn prepend-to-labels [label t]
  (if (and (vector? t)
           (= (first t) :label))
    [:label (apply vector label (second t))]
    t))

(defn rewrite-if-then [t]
  (if (and (vector? t)
           (= (first t) :if-then))
    (let [[_ tst then-clause] t]
      [:if-then
       [:statement [:label [0]] [:test-jump tst [:label [2]]]]
       [:statement [:label [1]] [:goto [:label [4]]]]
       then-clause])
    t))

(defn rewrite-if-then-else [t]
  (if (and (vector? t)
           (= (first t) :if-then-else))
    (let [[_ tst then-clause else-clause] t]
      [:if-then-else
       [:statement [:label [0]] [:test-jump tst [:label [2]]]]
       [:statement [:label [1]] [:goto [:label [4]]]]
       then-clause
       [:statement [:label [3]] [:goto [:label [5]]]]
       else-clause])
    t))

(defn rewrite-tree-labels [t]
  (if (vector? t)
    (case (first t)
          :statement
          (if (not= (first (second t)) :label)
            (let [[fst & rst] t]
              (apply vector fst [:label []] rst))
            t)
          :then-clause
          (w/postwalk (partial prepend-to-labels 2) t)
          :else-clause
          (w/postwalk (partial prepend-to-labels 4) t)
          :statements
          (let [[_ & ss] t
                statements (vec ss)
                scount     (count statements)]
            (if (> scount 1)
                (apply vector :statements
                       (for [idx (range scount)]
                         (w/prewalk
                          (partial prepend-to-labels idx)
                          (get statements idx))))
                t))
          t)
    t))

(defn rewrite-for [t]
  (if (and (vector? t)
           (= (first t) :for))
    (let [[_ [_ id] init end-val & [step]] t]
      [:for
       [:statement [:label [0]] [:assignment [:id id] init]]
       [:statement [:label [1]] [:store-for
                                 [:id id]
                                 [:label [2]]
                                 end-val
                                 (or step [:constant 1])]]])
    t))

(defn flatten-expression-sublist [t]
  (if (and (vector? t)
           (= (first t) :expression-list))
    (next t)
    [t]))

(defn flatten-expression-list [t]
  (if (vector? t)
    (let [[fst & expns] t]
      (vec (apply concat
                  [fst] (for [expn expns]
                          (flatten-expression-sublist expn)))))
    t))

(defn rewrite-on-goto [t]
  (if (and (vector? t)
           (= (first t) :on-goto))
    (let [[_ & rst]                  t
          [test-expr :as dest-exprs] (vec rst)]
      (apply vector :on-goto
             (vec (mapcat identity
                          (for [dest-idx (range 1 (count dest-exprs))]
                            (let [base-label (* 2 dest-idx)
                                  next-label (+ 2 base-label)]
                              [[:statement
                                [:label [base-label]]
                                [:test-jump
                                 [:<> test-expr [:constant dest-idx]]
                                 [:label [next-label]]]]
                               [:statement
                                [:label [(inc base-label)]]
                                [:goto (get dest-exprs dest-idx)]]]))))))
    t))

(defn rewrite-on-gosub [t]
  (if (and (vector? t)
           (= (first t) :on-gosub))
    (let [[_ & rst]                  t
          [test-expr :as dest-exprs] (vec rst)]
      (apply vector :on-gosub
             (vec (mapcat identity
                          (for [dest-idx (range 1 (count dest-exprs))]
                            (let [base-label (* 3 dest-idx)
                                  next-label (+ 3 base-label)
                                  end-label  (* 3 (count dest-exprs))]
                              [[:statement
                                [:label [base-label]]
                                [:test-jump
                                 [:<> test-expr [:constant dest-idx]]
                                 [:label [next-label]]]]
                               [:statement
                                [:label [(inc base-label)]]
                                [:gosub (get dest-exprs dest-idx)]]
                               [:statement
                                [:label [(+ 2 base-label)]]
                                [:goto [:label [end-label]]]]]))))))
    t))

(defn strip-control [t]
  (if (and (vector? t)
           (#{:then-clause :else-clause} (first t)))
    (let [[_ statements] t] (vec statements))
    t))

(defn collapse-statement-level [t]
  (if (and (vector? t)
           (#{:statements :if-then :if-then-else :for
              :on-goto :on-gosub} (first t)))
    (next t)
    [t]))

(def instructions #{:print :goto :test-jump})

(defn collapse-statements [t]
  (if (vector? t)
    (let [[fst & stmts] t]
      (vec (apply concat
                  [fst] (for [stmt stmts]
                          (collapse-statement-level stmt)))))
    t))

(defn collapse-nested-statement [t]
  (if (and (vector? t)
           (>= (count t) 2)
           (vector? (first (subvec t 2)))
           (= (ffirst (subvec t 2)) :statement))
    (nnext t)
    [t]))

(defn unnest-statements [t]
  (if (and (vector? t)
           (#{:statement :program} (first t)))
    (let [[fst & stmts] t]
      (vec (apply concat
                  [fst] (for [stmt stmts]
                          (collapse-nested-statement stmt)))))
    t))

(defn rewrite-line [t]
  (if (and (vector? t)
           (= (first t) :line))
    (let [[_ [_ label] statements-clause] t]
      #_(println "found :line")
      (w/postwalk (partial prepend-to-labels label) statements-clause))
    t))


(defn statementify [t]
  (if (and (vector? t)
           (= (first t) :statement))
    (let [[_ [_ label] [action & args]] t]
      {:label label :action action :args (vec args)})
    t))

(defn programmify [t]
  (if (and (vector? t)
           (= (first t) :program))
    (reduce (fn [acc {:keys [label] :as stmt}]
              (assoc acc label stmt))
            (avl/sorted-map-by compare-seq)
            (next t))
    t))

(defn proc-steps [t]
  (->> t
       (w/postwalk rewrite-cond-destination)
       (w/postwalk rewrite-if-then-else)
       (w/postwalk rewrite-if-then)
       (w/postwalk rewrite-for)
       (w/postwalk flatten-expression-list)
       (w/postwalk rewrite-on-goto)
       (w/postwalk rewrite-on-gosub)
       (w/postwalk rewrite-tree-labels)
       (w/postwalk strip-control)
       (w/postwalk rewrite-line)
       (w/postwalk collapse-statements)
       (w/postwalk unnest-statements)
       (w/postwalk statementify)
       (programmify)
       ))

(defn process [s]
  (-> s process-expressions))

(defn parse [s]
  (-> s
      basic
      process
      proc-steps))

(declare express)

(defn bbool [cxt exp]
  (let [v (express cxt exp)]
    (case v
      true  -1
      false  0
      0      0
      -1)))

(defn bfalse? [cxt exp]
  (let [v (bbool cxt (express cxt exp))]
    (= 0 v)))

(defn btrue? [cxt exp]
  (not (bfalse? cxt exp)))

(defn express [cxt exp]
  (if (coll? exp)
    (let [[typ a b] exp
          ;a (express cxt (fnext exp))
          ;b (express cxt (first (next (next exp))))
          ]
      (println "type:" typ "a:" a "b:" b)
      (case typ
        nil       nil
        :expression (express cxt a)
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

(defn action-assign [cxt args]
  (assoc-in cxt [:vars (second (first args))] (express cxt (fnext args))))


(defn action-jump [cxt label]
  (println "action-jump to" label)
  (-> cxt
      (assoc-in [:ip] (avl/subrange (:program cxt) >= label))
      (assoc-in [:jumped?] true)))

(defn action-goto [cxt [dest]]
  (println "action-goto to" dest)
  (let [dest-label (case (first dest)
                     :label (second dest)
                     [(express cxt dest)])]
    (println "jumping to" dest-label)
    (action-jump cxt dest-label)))

(defn action-gosub [cxt args]
  (action-goto
   (-> (update-in cxt [:substack] #(conj % (next (:ip cxt)))))
   args))

(defn action-store-for [cxt [[_ id] label end-expr step]]
  (assoc-in cxt [:for-map id]
            {:id id :label label :end-val end-expr :step step}))

(defn action-next [cxt args]
  ;; FIXME Update this for multiple arguments
  (let [[_ id] (first args)
        {:keys [label end-val step]} (get-in cxt [:for-map id])]
    (-> cxt
        (action-assign [[:id id] [:+ [:id id] step]])
        (#(if (btrue? % [:<= [:id id] end-val])
            (action-goto % [label])
            cxt)))))

(defn action-return [cxt _]
  (-> cxt
      (assoc-in [:ip] (peek (:substack cxt)))
      (assoc-in [:jumped?] true)
      (update-in [:substack] pop)))

(defn action-test-jump [cxt args]
  (if (btrue? cxt (first args))
    (action-goto cxt [(second args)])
    cxt))

(defn execute [cxt {:keys [action args] :as stmt}]
  (println "trying to execute " stmt)
  (println "action:" action "args:" args)
  (case action
    :assignment (action-assign cxt args)
    :print      (do
                  (println "OUTPUT:" (apply express cxt args))
                  cxt)
    :test-jump  (action-test-jump cxt args)
    :goto       (action-goto cxt args)
    :gosub      (action-gosub cxt args)
    :store-for  (action-store-for cxt args)
    :next       (action-next cxt args)
    :remark     cxt
    :return     (action-return cxt args)
    :end        (assoc-in cxt [:running?] false)))

(defn interpret [cxt line]
  (let [ast (first (vals  (process (basic line))))]
    (println "ast:" ast)
    (if (contains? ast :label)
      (store cxt ast)
      (execute cxt ast))))

(defn maybe-advance-ip [cxt]
  (if (:jumped? cxt)
    (assoc-in cxt [:jumped?] false)
    (update-in cxt [:ip] next)))

(defn run [cxt]
  (loop [cxt  (-> cxt
                  (assoc :ip (:program cxt))
                  (assoc :running? true)
                  (assoc :jumped? false)
                  (assoc :substack '())
                  (assoc :for-map {}))]
    (let [stmt (val (first (:ip cxt)))
          cxt  (->
                (execute cxt stmt)
                (maybe-advance-ip))]
      (if (and (:running? cxt) (:ip cxt))
        (recur cxt)
        (-> cxt (dissoc :ip :running? :jumped? :substack))))))

(defn srun [prog]
  (pp/pprint (->> prog
                  parse
                  (store-program {})
                  run)))
