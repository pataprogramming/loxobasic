(ns basic.parser
  (:require [instaparse.core :as ip]
            [clojure.edn]
            [clojure.data.avl :as avl]
            [clojure.walk :as w]
            [clojure.pprint :as pp]))

;;;; Test programs

(def ss "5  PRINT ;
         10 INPUT \"LET'S HAVE IT: \";D
         20 PRINT 3")

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

(def uu "10 K=3
         20 FOR I=0 TO K
         30 FOR J=5 TO I+6
         31 PRINT I
         32 PRINT J
         40 NEXT J,I")

(def vv "10  A=2:B=1
         20  ON A GOSUB 100,110,120
         30  ON B GOTO 50,60,70
         50  PRINT \"FOO\":STOP
         60  PRINT \"BAR\":STOP
         70  PRINT \"BAZ\":STOP
         100 B=1:A=1:RETURN
         110 B=2:A=1:RETURN
         120 B=3:A=1:RETURN")

(def ww "4  C=1
         5  B=C*2
         6  PRINT B
         10 PRINT \"ENTER A NUMBER:\"
         20 INPUT A
         30 PRINT \"YOU ENTERED:\"
         40 PRINT A")

(def xx "10 DATA \"A\",5
         15 DATA \"B\",10
         20 READ FOO,BAR,BAZ
         30 PRINT FOO
         40 PRINT BAR
         50 PRINT BAZ
         60 RESTORE
         70 READ WOMBLE
         80 PRINT WOMBLE
         90 END")

(def yy "5 REM HOWDY
         10 DATA 1,2,3,4")

(defn compare-seq [[a & as] [b & bs]]
  (if (and (nil? a) (nil? b))
    0
    (case (compare a b)
      -1 -1
      1  1
      0  (recur as bs))))

;;;; Parsing program text into AVL map

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
                  | input
                  | data
                  | read
                  | restore
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

    glue          = <';'>
    <print-list>  = (glue* | expression) (<ws>* (glue* | glue expression))*

    print         = <'PRINT' ws> print-list
    input         = <'INPUT' ws> (print-list glue)? id

    <notnl>       =#'[^\n\r]+'
    remark        = <'REM' notnl*>

    <datum>       = string | integer
    <data-list>   = datum (<ws* ',' ws*> datum)*
    data          = <'DATA' ws> data-list

    <id-list>     = id (<ws* ',' ws*> id)*
    read          = <'READ' ws> id-list

    restore       = <'RESTORE'>

    then-clause   = <'THEN' <ws>> (cond-destination | statements)
    cond-destination = expression
    else-clause   = <'ELSE' <ws>> (cond-destination | statements)

    if-then       = <'IF' <ws>> expression <ws> then-clause
    if-then-else  = <'IF' <ws>> expression <ws> then-clause <ws> else-clause

    for           = <'FOR' ws> id <ws*> <'=' ws*> expression <ws 'TO' ws> expression
                    (<ws 'STEP' ws> expression)?
    next          = <'NEXT' ws> id-list

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

;; (defn rewrite-input [t]
;;   (if (and (vector? t)
;;            (= (first t) :input))
;;     (let [[_ id] t]
;;       [:input
;;        [:statement [:label [0]] [:check-input]]
;;        [:statement [:label [1]] [:get-input id]]])
;;     t))

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
       ;;(w/postwalk rewrite-input)
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

;;;; Interpret and execute instructions

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
      #_(println "type:" typ "a:" a "b:" b)
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

(defn action-none [cxt _] cxt)

(defn reset-data-pointer [cxt]
  (let [ptr (drop-while #(not= (:action (val %)) :data) (:program cxt))]
    (assoc-in cxt [:data-pointer] [ptr (:args (second (first ptr)))])))

(defn advance-data-pointer [cxt]
  (assoc-in
   cxt [:data-pointer]
   (let [[ptr data] (:data-pointer cxt)]
     (if (seq (next data))
       [ptr (next data)]
       (if-let [next-ptr (drop-while #(not= (:action (second %)) :data) (next ptr))]
         [next-ptr (:args (second (first next-ptr)))]
         [nil nil])))))

(defn action-read [cxt args]
  (let [[id & rst] args
        [ptr [datum]] (:data-pointer cxt)]
    (cond
     (nil? id)  cxt
     (nil? ptr) (do (println "READ: OUT OF DATA ERROR") cxt)
     :else      (-> cxt
                    (#(action-assign % [id datum]))
                    (advance-data-pointer)
                    (action-read rst)))))

(defn action-restore [cxt _]
  (reset-data-pointer cxt))

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
  (let [[id & rst]               args
        {:keys [label end-val step]} (get-in cxt [:for-map (second id)])]
    (if id
      (let [cxt (action-assign cxt [id [:+ id step]])]
        (if (btrue? cxt [:<= id end-val])
          (action-goto cxt [label])
          (recur cxt rst)))
      cxt)))

(defn action-return [cxt _]
  (-> cxt
      (assoc-in [:ip] (peek (:substack cxt)))
      (assoc-in [:jumped?] true)
      (update-in [:substack] pop)))

(defn action-test-jump [cxt args]
  (if (btrue? cxt (first args))
    (action-goto cxt [(second args)])
    cxt))

(defn action-print [cxt args]
  ;; FIXME: Update for multiple args
  ;;(println "TRYING TO PRINT" args)
  ;;(println "EXPRESSION SHOULD BE" (express cxt (first args)))
  (let [strings    (apply str
                          (map (partial express cxt)
                               (filter #(not= % [:glue]) args)))
        newln      (if (= (last args) [:glue]) "" "\n")
        out-string (apply str strings newln)]
    (update-in cxt [:output] #(conj % out-string))))

(defn action-input [cxt args]
  (let [cxt (if (> (count args) 1)
              (action-print cxt (butlast args))
              cxt)]
    (if (empty? (:input cxt))
      (-> cxt
          (assoc-in [:input-blocked?] true)
          (assoc-in [:jumped?] true) ; Prevent IP advance
          )
      (let [id (last args)]
        ;; (println "INPUT RECEIVED:" (peek (:input cxt)))
        ;; (println "STORING TO:" id)
        (-> cxt
            (assoc-in [:input-blocked?] false)
            (#(action-assign % [id [:constant (peek (:input %))]]))
            (#(do
                (when (:echo-input? %)
                  (println (peek (:input %))))
                %))
            (update-in [:input] pop))))))

(defn execute [cxt {:keys [action args] :as stmt}]
  ;; (println "trying to execute " stmt)
  ;; (println "action:" action "args:" args)
  (case action
    :assignment (action-assign cxt args)
    :print      (action-print cxt args)
    :input      (action-input cxt args)
    :data       (action-none cxt args)
    :read       (action-read cxt args)
    :restore    (action-restore cxt args)
    :test-jump  (action-test-jump cxt args)
    :goto       (action-goto cxt args)
    :gosub      (action-gosub cxt args)
    :store-for  (action-store-for cxt args)
    :next       (action-next cxt args)
    :remark     (action-none cxt args)
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

;;;; Cheap console I/O for REPL testing

(defn show-output [cxt]
  (if (empty? (:output cxt))
    cxt
    (do
      ;; FIXME: Trailing ';' for print instead of println
      (print (peek (:output cxt)))
      (flush)
      (recur (update-in cxt [:output] pop)))))

(defn get-input [cxt]
  (if (:input-blocked? cxt)
    (update-in cxt [:input] #(conj % (read-line)))
    cxt))

;;;; Basic runners for REPL testing

(defn run [cxt]
  (loop [cxt  (-> cxt
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
                  (reset-data-pointer))]
    (let [stmt (val (first (:ip cxt)))
          cxt  (->
                (execute cxt stmt)
                (show-output)
                (get-input)
                (maybe-advance-ip))]
      (if (and (:running? cxt) (:ip cxt))
        (recur cxt)
        (-> cxt (dissoc :ip :running? :jumped? :substack))))))

(defn srun [prog]
  (pp/pprint (->> prog
                  parse
                  (store-program {})
                  run)))
