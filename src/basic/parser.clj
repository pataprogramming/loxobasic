(ns basic.parser
  (:require [instaparse.core :as ip]
            [clojure.edn]
            [clojure.data.avl :as avl]
            [clojure.walk :as w]
            [clojure.pprint :as pp]))

(def tt "10 A=1\n20 B=2\n10 C=3\n30 D=4\n50 D=5")

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
                  | if-then
                  | if-then-else
                  | goto
                  | gosub
                  | return
                  | end

    print         = <'PRINT' <ws>> expression
    <notnl>       =#'[^\n\r]+'
    remark        = <'REM' notnl*>
    then-clause   = <'THEN' <ws>> (cond-destination | statements)
    cond-destination = expression
    else-clause   = <'ELSE' <ws>> (cond-destination | statements)

    if            = if-then | if-then-else
    if-then       = <'IF' <ws>> expression <ws> then-clause
    if-then-else  = <'IF' <ws>> expression <ws> then-clause <ws> else-clause

    goto          = <'GOTO' <ws>> expression
    gosub         = <'GOSUB' <ws>> expression
    return        = <'RETURN'>
    end           = <'END'>

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
  (println "processing " parse-tree)
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
    :power_exp   treeify
    ;;:statement   (fn stmtify [[action & args]] {:action action :args (vec args)})
    ;; :statements  (fn sublabel [& statements]
    ;;                (for [idx (range (count statements))]
    ;;                  (update-in (nth statements idx)
    ;;                             [:label] #(conj % (* 10 idx)))))
    ;;;; :cond-destination
    ;;;; (fn cond-to-goto [expression]
    ;;;;   [{:label [0] :action :goto :args [expression]}])
    ;; :then-clause (fn thenlabel [statements]
    ;;                (label-clause-level 1 statements))
    ;; :else-clause (fn elselabel [statements]
    ;;                (label-clause-level 2 statements))
    ;; :line        (fn lineify [[_ label] statements]
    ;;                (println " STATEMENTS:" statements)
    ;;                (for [idx (range (count statements))]
    ;;                  (update-in (nth statements idx) [:label] #(conj % label))))

    ;; :program     (fn programmify [& lines]
    ;;                (reduce (fn [acc {:keys [label] :as line}] (assoc acc label line))
    ;;                        (avl/sorted-map-by compare-seq)
    ;;                        (mapcat identity lines)))
}
   parse-tree))


(defn maybe-statementify [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :statement))
    (let [[_ [action & args]] t]
      {:action action :args (vec args)})
    t))

(defn label-clause-level [level statements]
  (println "labeling: " statements "with: " level)
  (for [idx (range (count statements))]
    (update-in (nth statements idx)
               [:label] #(conj % level))))

(defn label-subtree-level [level t]
  (let [label-key (fn [m k] (if (contains? m k)
                             (update-in m [k]
                                        #(conj % level))
                             m))
        labeller  #(if (map? %)
                     (-> %
                         (label-key :label)
                         (label-key :destination))
                     %)]
    (w/postwalk labeller t)))

(defn rewrite-then [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :then-clause))
    (apply vector :statements (label-clause-level 2 (next (fnext t))))
    t))

(defn rewrite-else [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :else-clause))
    (apply vector :statements (label-clause-level 4 (next (fnext t))))
    t))

(defn maybe-rewrite-ifthen [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :statements)
           (map? (fnext t))
           (= (:action (fnext t)) :if-then))
    (let [[_ {:keys [args]}] t
          [tst then-stmts] args]
      [:statements
       {:action :test-jump :args [tst {:destination '(2)}] :label '(0)}
       (conj then-stmts {:action :jump :label '(3) :args [{:destination '(5)}]})])
    t))

(defn maybe-rewrite-ifthenelse [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :statements)
           (map? (fnext t))
           (= (:action (fnext t)) :if-then-else))
    (let [[_ {:keys [args]}] t
          [tst then-stmts else-stmts] args]
      [:statements
       {:action :test-jump :args [tst {:destination '(2)} {:destination '(4)}] :label '(0)}
       (conj then-stmts {:action :jump :label '(3) :args [{:destination '(5)}]})
       else-stmts])
    t))


(defn rewrite-statements [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :statements))
    (let [[_ & rst] t
          statements (vec rst)]
      (print "PP") (println statements)
      (for [idx (range (count statements))]
        (update-in (nth statements idx)
                   [:label] #(do
                               (conj % (* 10 idx))))))
    t))

;; :statements  (fn sublabel [& statements]
    ;;                (for [idx (range (count statements))]
    ;;                  (update-in (nth statements idx)
    ;;                             [:label] #(conj % (* 10 idx)))))


(defn rewrite-line-bad [t]
  (if (and (coll? t)
           (not (map? t))
           (= (first t) :line))
    (let [[_ [_ label] & rst] t
          [[_ & statements]]    rst]
      (pp/pprint statements)
      [:statements (for [idx (range (count statements))]
                     (update-in (nth statements idx) [:label] #(conj % label)))])
    t))

(defn rewrite-cond-destination [t]
  (if (and (vector? t)
           (= (first t) :cond-destination))
    [:statements [:statement [:goto (fnext t)]]]
    t))

(defn insert-second-when [t pred item]
  (if (pred t)
    (let [[fst & rst] t]
      (apply vector fst item rst))
    t))

(defn label-all-statements [t]
  (insert-second-when t #(and (vector? %)
                              (= (first %) :statement)
                              (not= (first (second %)) :label))
                      [:label []]))

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
    t
    ))

(defn strip-then-else [t]
  (if (and (vector? t)
           (or (= (first t) :then-clause)
               (= (first t) :else-clause)
               ))
    (let [[_ statements] t] (vec statements))
    t))

(defn collapse-statement-level [t]
  (if (and (vector? t)
           (#{:statements :if-then :if-then-else} (first t)))
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
      {:label label :action action :args args})
    t))

(defn programmify [t]
  (if (and (vector? t)
           (= (first t) :program))
    (reduce (fn [acc {:keys [label] :as stmt}]
              (assoc acc label stmt))
            (avl/sorted-map-by compare-seq)
            (next t))
    t)
)
; :program     (fn programmify [& lines]
    ;;                (reduce (fn [acc {:keys [label] :as line}] (assoc acc label line))
    ;;                        (avl/sorted-map-by compare-seq)
    ;;                        (mapcat identity lines)))


(defn proc-steps [t]
  (->> t
       (w/postwalk rewrite-cond-destination)
       (w/postwalk rewrite-if-then-else)
       (w/postwalk rewrite-tree-labels)
       (w/postwalk strip-then-else)
       (w/postwalk rewrite-line)
       (w/postwalk collapse-statements)
       (w/postwalk unnest-statements)
       (w/postwalk statementify)
       (programmify)
       ;; All parsed control forms have been rewritten to statements
))

;; (defn postwalk-nonmap [t]
;;   (cond (or (nil? t) (empty? t))
;;         t
;;         (or
;;          (and (coll? t) (not (map? t)))
;;          (seq? t))
;;         (do
;;           (postwalk-nonmap (first t))
;;           (println "looking at: " t))))

(defn process-conditionals [s]
  (println "processing " s)
  (ip/transform {:constant vector} s))
  ;; (ip/transform {:goto #([])}
   ;; {

    ;;:if-clause   #(label-clause-level 0 %)
    ;;:if-then       #(label-clause-level 0 %)
    ;; :cond-destination (fn cond-to-goto [expression] {:action :goto :args [expression]})
    ;; :then-clause      #(label-clause-level 1 [%])
    ;; :else-clause      #(label-clause-level 2 [%])
    ;; }
;;    s)
;; )
(defn process [s]
  (-> s process-expressions))

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

(defn action-jump [cxt dest-pair]
  (-> cxt
      (assoc-in [:ip] (avl/subrange (:program cxt) >= dest-pair))
      (assoc-in [:jumped?] true)))

(defn action-goto [cxt args]
  (let [dest-label (express cxt (first args))]
    (action-jump [dest-label 0])))

(defn action-gosub [cxt args]
  (action-goto
   (-> (update-in cxt [:substack] #(conj % (next (:ip cxt)))))
   args))

(defn action-return [cxt _]
  (-> cxt
      (assoc-in [:ip] (peek (:substack cxt)))
      (assoc-in [:jumped?] true)
      (update-in [:substack] pop)))

(defn execute [cxt {:keys [action args] :as stmt}]
  (println "trying to execute " stmt)
  (println "action:" action "args:" args)
  (case action
    :assignment
    (assoc-in cxt [:vars (second (first args))] (express cxt (fnext args)))
    :print  (do
              (println "OUTPUT:" (apply express cxt args))
              cxt)
    :if     (if (btrue? cxt (first args))
              (action-goto cxt [(second args)])
              (if (= (count args) 3)
                (action-goto cxt [(nth args 2)])
                cxt))
    :goto   (action-goto cxt args)
    :gosub  (action-gosub cxt args)
    :remark cxt
    :return (action-return cxt args)
    :end    (assoc-in cxt [:running?] false)))

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
                  (assoc :substack '()))]
    (let [stmt (val (first (:ip cxt)))
          cxt  (->
                (execute cxt stmt)
                (maybe-advance-ip))]
      (if (and (:running? cxt) (:ip cxt))
        (recur cxt)
        (-> cxt (dissoc :ip :running? :jumped? :substack))))))
