(ns basic.parser
  (:require [instaparse.core :as ip]
            [clojure.edn]
            [clojure.data.avl :as avl]
            [clojure.walk :as w]
            [clojure.pprint :as pp]))

;;;; Test programs

(def oo "10  PRINT \"ABS(-5)=\";ABS(-5)
         20  PRINT \"ASC(FOO)=\";ASC(\"FOO\")
         30  PRINT \"ATN(25)=\";ATN(2.5)
         40  PRINT \"CHR$(69)=\";CHR$(69)
         50  PRINT \"COS(1)=\";COS(1)
         60  PRINT \"EXP(2)=\";EXP(2)
         70  PRINT \"INT(4.5)=\";INT(4.5)
         80  PRINT \"LEFT$('BAR',2)=\";LEFT$(\"BAR\",2)
         90  PRINT \"LEN('WEASEL')=\";LEN(\"WEASEL\")
         100 PRINT \"LOG(2.178)=\";LOG(2.178)
         110 PRINT \"MID$('FOOBARBAZ',3,4)=\";MID$(\"FOOBARBAZ\",3,4)
         120 PRINT \"RND(1)=\";RND(1)
         130 PRINT \"RIGHT$('FOOBARBAZ',3)=\";RIGHT$(\"FOOBARBAZ\",3)
         140 PRINT \"SGN(-5)=\";SGN(-5)
         150 PRINT \"STR$(8.45)=\";STR$(8.45)
         160 PRINT \"SQR(100)=\";SQR(100)
         170 PRINT \"FOO\";TAB(10);\"FOO\"
         180 PRINT \"TAN(10)=\";TAN(10)
         190 PRINT \"VAL('500.2')=\";VAL(\"500.2\")")

(def pp "10 A=2.2
         20 B=0.1
         30 C=1
         40 PRINT A+B+C")

(def qq "10 DIM A(5)
         15 X=2
         20 DIM B(X,X,3)
         25 B(1,1,2)=99
         30 FOR N=1 TO 5
         40 A(N)=10-N
         50 NEXT N
         60 FOR N=1 TO 5
         70 PRINT A(N);\" \";
         80 NEXT N
         90 PRINT")

(def rr "5  P=-1
         10 DEF F(P,Q)=(P+Q)*G(P)
         15 DEF G(Q)=P*Q
         20 PRINT F(5,4)
         30 REM OUTPUT SHOULD BE 45")

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
         70 PRINT A;\" \";B;\" \";C;\" \";D;\" \";E
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
   "<root>        = program
                  | <ws*> directive <ws* nl*>
                  | <ws*> line <ws* nl*>
                  | <ws*> empty-line <ws* nl*>
    directive     = statement
    program       = lines

    empty-line    = label
    <lines>       = lines <nl+> line <nl*>
                  | line <nl*>
    line          = <ws*> label <ws*> statements <ws*>
    label         = integer
    statements    = statement-list
    <statement-list>  = statement-list <ws* ':' ws*> statement
                  | statement
    statement     = assignment
                  | def
                  | dim
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
                  | load
                  | dump
                  | quit
                  | reset
                  | run

    <expression-list> = expression (<ws* ','> expression)*
    param-id      = id <'(' ws*> expression-list <ws* ')'>


    parameters    = id-list
    def           = <'DEF' ws*> id <ws* '(' ws*> parameters <ws* ')' ws* '=' ws*> expression
    dim-arg      = id <ws* '(' ws*> arguments <ws* ')'>
    dim           = <'DIM' ws> dim-arg (<ws* ',' ws*> dim-arg)*

    tight-glue    = <' '>
    glue          = <';'>
    <print-list>  = (glue* | expression) ((tight-glue* | glue*) expression)* glue?

    print         = <'PRINT'> (<ws*> print-list)?
    input         = <'INPUT' ws> (print-list glue)? id

    <notnl>       =#'[^\n\r]+'
    remark        = <'REM' notnl*>

    <datum>       = string | number
    <data-list>   = datum (<ws* ',' ws*> datum)*
    data          = <'DATA' ws> data-list

    <id-list>     = (id | id-call) (<ws* ',' ws*> (id | id-call))*    read          = <'READ' ws> id-list

    restore       = <'RESTORE'>

    then-clause   = <'THEN' <ws>> (cond-destination | statements)
    cond-destination = expression
    else-clause   = <'ELSE' <ws>> (cond-destination | statements)

    if-then       = <'IF' <ws>> expression <ws> then-clause
    if-then-else  = <'IF' <ws>> expression <ws> then-clause <ws> else-clause

    for           = <'FOR' ws> id <ws*> <'=' ws*> expression <ws 'TO' ws> expression
                    (<ws 'STEP' ws> expression)?
    next          = <'NEXT' ws> id-list

    goto          = <'GOTO' ws> expression
    gosub         = <'GOSUB' ws> expression
    return        = <'RETURN'>
    end           = <'END' | 'STOP'>

    reset         = <'RESET'>
    run           = <'RUN'>
    <filename>    = string
    load          = <'LOAD' ws> filename
    dump          = <'DUMP'>
    quit          = <'QUIT'>

    on-goto       = <'ON' ws> expression <ws 'GOTO' ws> expression-list
    on-gosub       = <'ON' ws> expression <ws 'GOSUB' ws> expression-list

    assignment    = <('LET' <ws>)?> (id | id-call) <ws*> <'='> <ws*> expression

    constant      = number | string
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
                  | id-call
                  | constant

    <arguments> = expression (<ws* ',' ws*> expression)*
    id-call       = id <ws* '(' ws*> arguments <ws* ')'>

    integer = #'[0-9]+'
    number = #'([0-9]+(\\.[0-9]*)?)|(\\.[0-9]+)'
    <digit> = #'[0-9]'
    <string> = <'\"'> #'[^\"]*' <'\"'>
    <alpha> = #'[A-Za-z]'
    alphanum = #'[A-Za-z0-9]'
    id       = #'[A-Za-z][A-Za-z0-9$]*'
    nl = ws* #'[\n\r]+' (ws* | #'[\n\r]*')
    ws = #'[ \t\f]+'"
   :string-ci true))

(defn treeify
  ([a]
     a)
  ([a b]
     [(keyword a) b])
  ([a b c]
     [(keyword b) a c]))

(defn process-number-string [s]
  (clojure.edn/read-string
   (cond (= (first s) \.) (str "0" s)
         (= (last s) \.)  (str s "0")
         :else            s)))

(defn process-expressions [parse-tree]
  (ip/transform
   {:number      (comp process-number-string str)
    :integer     (comp clojure.edn/read-string str)
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

(defn rewrite-def [t]
  (if (and (vector? t)
           (= (first t) :def))
    (let [[_ [_ id] [_ & parameters] body] t]
      [:def [:id id] parameters body])
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
  (if (ip/failure? t)
    (println "FAILED PARSING, SKIPPING PROC-STEPS")
    (->> t
         (w/postwalk rewrite-cond-destination)
         (w/postwalk rewrite-if-then-else)
         (w/postwalk rewrite-if-then)
         (w/postwalk rewrite-for)
         (w/postwalk rewrite-def)
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
         )))

(defn process [s]
  (-> s process-expressions))

(defn parse [s]
  (-> s
      basic
      first
      process
      proc-steps))

;;;; Builtins

(defn make-builtin [cxt [name params function]]
  (assoc-in cxt [:symbols [:id name]]
            {:kind     :builtin
             :params   (vec (map #(vector :id %) params))
             :function function}))

(def builtins
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



;;;; Interpret and execute instructions

(defn set-error [cxt err-string & trace]
  (-> cxt
      (assoc :error err-string)
      (assoc :trace trace)
      (assoc :running false)))

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

(defn call-builtin [cxt {:keys [function]} resolved-args scope]
  (apply function resolved-args))

(defn call-function [cxt func resolved-args scope]
  (let [formal-params     (:parameters func)
        param-values      (map (comp #(hash-map :kind  :constant
                                                :value % )
                                     resolved-args))
        new-scope         (zipmap formal-params param-values)
        body              (:body func)]
    ;; (println "WITHIN CALL, RESOLVING" body "WITH" new-scope)
    ;; (println "FORMAL PARAMS ARE" formal-params)
    ;; (println "SUPPLIED ARGUMENTS ARE" params)
    ;; (println "RESOLVED ARGUMENTS ARE" param-values)
    (express cxt body new-scope)))

(defn resolve-call [cxt [_ id & params :as exp] & [scope]]
  ;; (println "RESOLVING CALL TO" id "WITH" params)
  ;; (println "EXPRESSION" exp)
  ;; (println "SYMBOL" (get-in cxt [:symbols id]))
  (let [{:keys [kind] :as entry} (get-in cxt [:symbols id])
        resolved-args            (map #(express cxt % scope) params)]
    (case kind
      :function (call-function cxt entry resolved-args scope)
      :builtin  (call-builtin cxt entry resolved-args scope)
      :array    (let [{:keys [value]}
                      (get-in cxt (concat [:symbols id :matrix]
                                          resolved-args))]
                  value)
      (do (println "UNKNOWN ID TYPE" kind)))))

(defn resolve-id [cxt id & [scope]]
  ;; (when scope (println "RESOLVE" id "IN" scope))
  (let [{:keys [kind] :as entry} (if (contains? scope id)
                                   (get scope id)
                                   (get-in cxt [:symbols id]))]
    (case kind
      :constant (:value entry)
      (do (println "UNKNOWN ID TYPE" kind)))))

(defn express [cxt exp & [scope]]
  (if (coll? exp)
    (let [[typ a b] exp
          ;;a (express cxt (fnext exp))
          ;;b (express cxt (first (next (next exp))))
          expr (fn expr [e] (express cxt e scope))]
      #_(println "type:" typ "a:" a "b:" b)
      (case typ
        nil       nil
        :expression (expr a)
        :constant a
        :id       (resolve-id cxt exp scope)
        :id-call  (resolve-call cxt exp scope)
        :+        (+ (expr a) (expr b))
        :-        (if (not (nil? (expr b)))
                    (- (expr a) (expr b))
                    (- (expr a)))
        :*        (* (expr a) (expr b))
        :/        (/ (expr a) (expr) b)
        :AND      (bbool cxt (and (btrue? cxt a) (btrue? cxt b)))
        :OR       (bbool cxt (or  (btrue? cxt a) (btrue? cxt b)))
        :NOT      (bbool cxt (not (btrue? cxt a)))
        :=        (bbool cxt (= (expr a) (expr b)))
        :>        (bbool cxt (> (expr a) (expr b)))
        :<        (bbool cxt (< (expr a) (expr b)))
        :<=       (bbool cxt (<= (expr a) (expr b)))
        :>=       (bbool cxt (>= (expr a) (expr b)))
        :<>       (bbool cxt (not= (expr a) (expr b)))
        :><       (bbool cxt (not= (expr a) (expr b)))
        (throw (Throwable. (str "can't handle" typ)))
        ))
    exp))

(defn store [cxt line]
  (assoc-in cxt [:program (:label line)] line))

(defn store-program [cxt program]
  (assoc cxt :program program))

(defn action-assign [cxt args]
  (case (ffirst args)
    :id      (let [[id & [exp]] args]
               (assoc-in cxt [:symbols id]
                         {:kind  :constant
                          :value (express cxt exp)}))
    :id-call (let [[[_ id & params] & [exp]] args
                   resolved                  (map (partial express cxt)
                                                  params)]
               ;; (println "ARRAY ASSIGNMENT TO" id "PATH" resolved)
               ;; (println "FULL ARGS" args)
               (assoc-in cxt (concat [:symbols id :matrix] resolved)
                         {:kind  :constant
                          :value (express cxt exp)}))))

(defn action-def [cxt [id parameters body]]
  (assoc-in cxt [:symbols id]
            {:kind :function
             :parameters parameters
             :body body}))

(defn action-dim [cxt & [[[_ id & dims]] & rst]]
  ;; (println "ID" id "DIMS" dims "RST" rst)
  (if id
    (let [resolved (map (partial express cxt) dims)
          matrix   (vec (reduce #(vec (repeat (inc %2) %1)) nil resolved))]
      (recur (assoc-in cxt [:symbols id]
                       {:kind       :array
                        :dimensions (count dims)
                        :matrix     matrix})
             rst))
    cxt))

(defn action-none [cxt _] cxt)

(defn reset-data-pointer [cxt]
  (if (:program cxt)
    (let [ptr (drop-while #(not= (:action (val %)) :data) (:program cxt))]
      (assoc-in cxt [:data-pointer] [ptr (:args (second (first ptr)))]))
    cxt))

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
  ;; (println "action-jump to" label)
  (-> cxt
      (assoc-in [:ip] (avl/subrange (:program cxt) >= label))
      (assoc-in [:jumped?] true)))

(defn action-goto [cxt [dest]]
  ;; (println "action-goto to" dest)
  (let [dest-label (case (first dest)
                     :label (second dest)
                     [(express cxt dest)])]
    ;;(println "jumping to" dest-label)
    (action-jump cxt dest-label)))

(defn action-gosub [cxt args]
  (action-goto
   (-> (update-in cxt [:substack] #(conj % (next (:ip cxt)))))
   args))

(defn action-store-for [cxt [[_ id] label end-expr step]]
  (assoc-in cxt [:for-map id]
            {:id id :label label :end-val end-expr :step step}))

(defn action-next [cxt args]
  (let [[id & rst]                   args
        {:keys [label end-val step]} (get-in cxt [:for-map (second id)])]
    (if id
      (let [cxt (action-assign cxt [id [:+ id step]])]
        (if (btrue? cxt [:OR
                         [:AND [:> step 0] [:<= id end-val]]
                         [:AND [:< step 0] [:>= id end-val]]
                         [:= id end-val]])
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
  ;;(println "TRYING TO PRINT" args)
  ;;(println "EXPRESSION SHOULD BE" (express cxt (first args)))
  (let [strings    (apply str
                          (map (partial express cxt)
                               (map #(case %
                                       [:glue] " "
                                       [:tight-glue] " "
                                       %) args)))
        newln      (if (= (last args) [:glue]) "" "\n")
        out-string (apply str strings newln)]
    (update-in cxt [:output] #(conj % out-string))))

(defn action-input [cxt args]
  (let [cxt (if (> (count args) 1)
              (action-print cxt (concat (butlast args) ["?"] [:glue]))
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

(defn action-reset [cxt & _]
  (-> cxt
      (dissoc [:ip :data-pointer :running? :jumped? :substack
               :for-map :output :input :input-blocked?
               :program :symbols])
      (generate-builtins)))

(defn action-load [cxt & [filename]]
  (let [file    (str "resources/bcg/" (first filename) ".bas")
        ;;_       (println "TRYING TO LOAD FROM" file)
        prog    (slurp file)
        ;;_       (println "READ IN\n" prog)
        p1      (basic prog)
        ;;_       (println "PARSE1")
        ;;_       (pp/pprint p1)
        p2      (if (ip/failure? p1) p1 (process (first p1)))
        ;;_       (pp/pprint p2)
        parsed  (if (ip/failure? p2) p2 (proc-steps p2))
        ]
    (if (ip/failure? parsed)
      (-> cxt
          (update-in [:output] #(conj % parsed)))
      (-> cxt
          (action-reset)
          (store-program parsed)))))

(declare run)

(defn action-run [cxt & _]
  (run cxt))

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
    :def        (action-def cxt args)
    :dim        (action-dim cxt args)
    :remark     (action-none cxt args)
    :return     (action-return cxt args)
    :reset      (action-reset cxt args)
    :load       (action-load cxt args)
    :run        (action-run cxt args)
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
