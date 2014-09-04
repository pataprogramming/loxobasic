(ns basic.parser
  (:require [basic.test :refer [compare-seq]]
            [basic.util :refer [def-]]
            [clojure.data.avl :as avl]
            [clojure.walk :as w]
            [instaparse.core :as ip]))

;;;; Parsing program text into AVL map

(def- basic
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

    if-then       = <'IF' <ws*>> expression <ws*> then-clause
    if-then-else  = <'IF' <ws*>> expression <ws*> then-clause <ws*> else-clause

    for           = <'FOR' ws> id <ws*> <'=' ws*> expression <ws* 'TO' ws*> expression
                    (<ws* 'STEP' ws*> expression)?
    next          = <'NEXT' ws> id-list

    goto          = <'GOTO' ws*> expression
    gosub         = <'GOSUB' ws*> expression
    return        = <'RETURN'>
    end           = <'END' | 'STOP'>

    reset         = <'RESET'>
    run           = <'RUN'>
    <filename>    = string
    load          = <'LOAD' ws> filename
    dump          = <'DUMP'>
    quit          = <'QUIT'>

    on-goto       = <'ON' ws*> expression <ws* 'GOTO' ws*> expression-list
    on-gosub       = <'ON' ws*> expression <ws* 'GOSUB' ws*> expression-list

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

    integer = #'-?[0-9]+'
    number = #'-?([0-9]+(\\.[0-9]*)?)|(\\.[0-9]+)'
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

(defn- rewrite-for [t]
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

(defn- flatten-expression-sublist [t]
  (if (and (vector? t)
           (= (first t) :expression-list))
    (next t)
    [t]))

(defn- flatten-expression-list [t]
  (if (vector? t)
    (let [[fst & expns] t]
      (vec (apply concat
                  [fst] (for [expn expns]
                          (flatten-expression-sublist expn)))))
    t))

(defn- rewrite-on-goto [t]
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

(defn- rewrite-on-gosub [t]
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

(defn- rewrite-def [t]
  (if (and (vector? t)
           (= (first t) :def))
    (let [[_ [_ id] [_ & parameters] body] t]
      [:def [:id id] parameters body])
    t))

(defn- strip-control [t]
  (if (and (vector? t)
           (#{:then-clause :else-clause} (first t)))
    (let [[_ statements] t] (vec statements))
    t))

(defn- collapse-statement-level [t]
  (if (and (vector? t)
           (#{:statements :if-then :if-then-else :for
              :on-goto :on-gosub} (first t)))
    (next t)
    [t]))

(def- instructions #{:print :goto :test-jump})

(defn- collapse-statements [t]
  (if (vector? t)
    (let [[fst & stmts] t]
      (vec (apply concat
                  [fst] (for [stmt stmts]
                          (collapse-statement-level stmt)))))
    t))

(defn- collapse-nested-statement [t]
  (if (and (vector? t)
           (>= (count t) 2)
           (vector? (first (subvec t 2)))
           (= (ffirst (subvec t 2)) :statement))
    (nnext t)
    [t]))

(defn- unnest-statements [t]
  (if (and (vector? t)
           (#{:statement :program} (first t)))
    (let [[fst & stmts] t]
      (vec (apply concat
                  [fst] (for [stmt stmts]
                          (collapse-nested-statement stmt)))))
    t))

(defn- rewrite-line [t]
  (if (and (vector? t)
           (= (first t) :line))
    (let [[_ [_ label] statements-clause] t]
      #_(println "found :line")
      (w/postwalk (partial prepend-to-labels label) statements-clause))
    t))


(defn- statementify [t]
  (if (and (vector? t)
           (= (first t) :statement))
    (let [[_ [_ label] [action & args]] t]
      {:label label :action action :args (vec args)})
    t))

(defn- programmify [t]
  (if (and (vector? t)
           (= (first t) :program))
    (reduce (fn [acc {:keys [label] :as stmt}]
              (assoc acc label stmt))
            (avl/sorted-map-by compare-seq)
            (next t))
    t))

(defn- proc-steps [t]
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

(defn- process [s]
  (-> s
      process-expressions
      proc-steps))

(defn parse [s]
  (-> s
      basic
      first
      process))
