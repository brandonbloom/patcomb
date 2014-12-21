(ns patcomb.core)

(defn head [x]
  (if (seq? x)
    (first x)
    (class x)))

(defmulti pat->proc (fn [form subject] (head form)))

(defmethod pat->proc 'const
  [[_ x] subject]
  [[:test `(= ~x ~subject)]])

(defmethod pat->proc 'blank
  [[_ sym] subject]
  [[:bind sym subject]])

(defmethod pat->proc 'as
  [[_ sym subpat] subject]
  (concat (pat->proc (list 'blank sym) subject)
          (pat->proc subpat subject)))

(defmethod pat->proc java.lang.Long
  [value subject]
  (pat->proc (list 'const value) subject))

(defmethod pat->proc clojure.lang.PersistentVector
  [subpats subject]
  (let [n (count subpats)]
    (concat [[:test `(vector? ~subject)]
             [:test `(= ~n (count ~subject))]]
            (for [[i subpat] (map vector (range n) subpats)
                  :let [sym (symbol (str subject "__" i))]
                  foo (cons [:bind sym `(nth ~subject ~i)]
                            (pat->proc subpat sym))]
              foo))))

(defmethod pat->proc 'alt
  [[_ & subpats] subject]
  [(into [:alt] (map #(pat->proc % subject) subpats))])

(defmethod pat->proc 'rule
  [[_ lhs rhs] subject]
  (concat (pat->proc lhs subject)
          [[:return rhs]]))

(defmethod pat->proc 'guard
  [[_ pattern expr] subject]
  (concat (pat->proc pattern subject)
          [[:test expr]]))

(defmethod pat->proc 'check
  [[_ pattern pred] subject]
  (assert (symbol? pred))
  (let [temp (symbol (str subject "__" pred))]
    (concat (pat->proc pattern subject)
            [[:bind temp subject]
             [:test (list pred temp)]])))

(defmulti cmd->clj (fn [k [op & args]] op))

(def ^:dynamic *names* nil)

(defn proc->clj [proc k]
  (binding [*names* (or *names* #{})]
    (let [proc* (concat proc [[:success]])]
      ((reduce cmd->clj k proc*) nil))))

(defmethod cmd->clj :bind
  [k [_ sym init]]
  (if-let [existing (*names* sym)]
    (fn [x] (k `(when (= ~existing ~init) ~x)))
    (do
      (set! *names* (conj *names* sym))
      (fn [x] (k `(let [~sym ~init] ~x))))))

(defmethod cmd->clj :test
  [k [_ expr]]
  (fn [x] (k `(when ~expr ~x))))

(defmethod cmd->clj :alt
  [k [_ & subprocs]]
  (fn [x] (k `(or ~@(mapv #(proc->clj % identity) subprocs)))))

(defmethod cmd->clj :return
  [k [_ expr]]
  (fn [_] (k expr)))

(defmethod cmd->clj :success
  [k _]
  (let [names *names*]
    (fn [_]
      (k (into {} (for [sym names
                        :when (not (re-find #"^__" (str sym)))]
                    [(list 'quote sym) sym]))))))

(defn match->clj
  [pattern subject]
  (let [proc (pat->proc pattern '__subject)
        k (fn [x] `(let [~'__subject ~subject] ~x))]
    (proc->clj proc k)))

(defn match [pattern subject]
  (eval (match->clj pattern subject)))

(comment

  (require '[clojure.test :refer [is are]])

  (are [pattern subject result]
       (= (match pattern subject) result)

       ;; explicit literal values
       '(const 5)    5     {}
       '(const 5)    0     nil

       ;; numbers are already literal
       '5     5     {}
       '5     0     nil

       ;;; named blanks
       '(blank x)   5   {'x 5}

       ;;; named patterns
       '(as x 1)    1   {'x 1}

       ;;; vectors of subpatterns
       '[]                        []          {}
       '[1]                       []          nil
       '[(blank x) 10]            [5 10]      {'x 5}
       '[(blank x) 10]            [5 11]      nil
       '[(blank x) (blank x)]     [3 3]       {'x 3}
       '[(blank x) (blank x)]     [3 5]       nil

       ;;; ordered choice
       '(alt (as x 1) (as y 2))         1       {'x 1}
       '(alt (as x 1) (as y 2))         2       {'y 2}
       '(alt (as x 1) (as y 2))         3       nil
       '[(blank x) (alt (as y 1) 2)]    [1 2]   {'x 1}

       ;;; rules
       '(rule [(blank x) (blank y)] [y x])     [1 2]       [2 1]

       ;;; guard expressions
       '(guard (blank x) (< 3 x 5))    4    {'x 4}
       '(guard (blank x) (< 3 x 5))    7    nil

       ;; predicate checks
       '(check (blank x) odd?)    3   {'x 3}
       '(check (blank x) odd?)    4   nil

       )

  (->
    ;5
    ;'(rule [(blank x) (blank y)] [y x])
    '[1 (blank x) [3] (blank x) (alt (as y 1) 2)]
    ;'[(blank x) (blank x)]
    ;'(alt (as x 1) (as y 2))

    ;(pat->proc 'foo)
    ;(fipp.edn/pprint {:width 150})

    (match->clj 'foo)
    (fipp.clojure/pprint {:width 150})

    )

)

