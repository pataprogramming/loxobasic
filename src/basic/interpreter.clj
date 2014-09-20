(ns basic.interpreter
  (:require [basic.builtins :refer [generate-builtins]]
            [basic.parser :refer [parse]]
            [clojure.data.avl :as avl]
            [instaparse.core :as ip]))

(defn error? [cxt]
  (contains? cxt :error))

;;;; Input and output
;;;; Handlers must be supplied by the user

(defn perform-output! [cxt handler]
  (if (empty? (:output cxt))
    cxt
    (recur (handler cxt) handler)))

(defn perform-input! [cxt handler!]
  (if (:input-blocked? cxt)
    (handler! cxt)
    cxt))

(defn perform-io-or-error! [cxt in-handler out-handler err-handler]
  (if (error? cxt)
    (err-handler cxt)
    (-> cxt
        (perform-output! out-handler)
        (perform-input! in-handler))))

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
  (update-in cxt [:program] #(merge program))
  ;(assoc cxt :program program)
  )

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
      (assoc-in [:advance?] false)))

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
      (assoc-in [:advance?] false)
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
          (assoc-in [:advance?] false)
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
      (dissoc [:ip :data-pointer :running? :advance? :substack
               :for-map :output :input :input-blocked?
               :program :symbols])
      (generate-builtins)))

(defn action-load [cxt & [filename]]
  (let [file    (str "resources/bcg/" (first filename) ".bas")
        ;;_       (println "TRYING TO LOAD FROM" file)
        prog    (slurp file)
        ;; ;;_       (println "READ IN\n" prog)
        ;; p1      (basic prog)
        ;; ;;_       (println "PARSE1")
        ;; ;;_       (pp/pprint p1)
        ;; p2      (if (ip/failure? p1) p1 (process (first p1)))
        ;; ;;_       (pp/pprint p2)
        ;;parsed  (if (ip/failure? p2) p2 (proc-steps p2))
        parsed   (parse prog)
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
  (let [ast (first (vals  (parse line)))]
    #_(println "ast:" ast)
    (if (contains? ast :label)
      (store cxt ast)
      (execute cxt ast))))

(defn maybe-advance-ip [cxt]
  (if (:advance? cxt)
    (-> cxt
        (update-in [:ip] next))
    (assoc-in cxt [:advance?] true)))

;; FIXME: Add the 'step' function here. It should take
;; input and output handler functions that operate on
;; the cxt structure

(defn clear-error [cxt]
  (dissoc cxt :error :trace))

(defn initialize [cxt]
  (-> cxt
      (assoc :ip (:program cxt))
      (assoc :data-pointer [nil nil])
      (assoc :running? true)
      (assoc :advance? true)
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

(defn run [cxt in-handler out-handler err-handler]
  (loop [cxt (initialize cxt)]
    (let [cxt (-> cxt
                  (step)
                  (perform-io-or-error! in-handler out-handler err-handler)
                  (maybe-advance-ip)
                  ;;(assoc-in [:running?] false) ;; FIXME: Not Terminating
                  )]
      (if (and (:running? cxt) (:ip cxt))
        (recur cxt)
        (-> cxt ;(dissoc :ip :running? :advance? :substack)
            )))))
