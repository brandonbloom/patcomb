(ns patcomb.core
  (:refer-clojure :exclude [compile]))

(defn head [x]
  (if (seq? x)
    (first x)
    (class x)))

(defmulti -match (fn [form env subject] (head form)))
(defmulti -rewrite (fn [form env subject] (head form)))

(defmethod -match 'const
  [[_ x] env subject]
  (when (= x subject)
    env))

(defmethod -match 'blank
  [[_ sym] env subject]
  (assert (symbol? sym))
  (if-let [x (env sym)]
    (when (= x subject)
      env)
    (assoc env sym subject)))

(defmethod -match 'as
  [[_ sym subpat] env subject]
  (assert (symbol? sym))
  (when-let [env* (-match subpat env subject)]
    (assoc env* sym subject)))

(defmethod -match java.lang.Long
  [value env subject]
  (-match (list 'const value) env subject))

(defmethod -match clojure.lang.PersistentVector
  [subpats env subject]
  (when (and (vector? subject) (= (count subpats) (count subject)))
    (reduce (fn [env [pattern subject]]
              (-match pattern env subject))
            env
            (map vector subpats subject))))

(defmethod -match 'alt
  [[_ & subpats] env subject]
  (some #(-match % env subject) subpats))

(defmethod -rewrite 'rule
  [[_ lhs rhs] env subject]
  (when-let [env (-match lhs env subject)]
    (eval `(let [~@(mapcat (fn [[k v]] [k (list 'quote v)]) env)]
             ~rhs))))

(defmethod -rewrite 'alt
  [[_ & subpats] env subject]
  (some #(-rewrite % env subject) subpats))

(defn match [pattern subject]
  (-match pattern {} subject))

(defn rewrite [subject strategy]
  (-rewrite strategy {} subject))

(comment

  (require '[clojure.test :refer [is are]])

  (are [pattern subject substitutions]
       (= (match pattern subject) substitutions)

       ;; explicit literal values
       '(const 5)                 5           {}
       '(const 5)                 0           nil

       ;; numbers are already literal
       '5                         5           {}
       '5                         0           nil

       ;; named blanks
       '(blank x)                 5           {'x 5}

       ;; named patterns
       '(as x 1)                  1           {'x 1}

       ;; vectors of subpatterns
       '[]                        []          {}
       '[1]                       []          nil
       '[(blank x) 10]            [5 10]      {'x 5}
       '[(blank x) 10]            [5 11]      nil
       '[(blank x) (blank x)]     [3 3]       {'x 3}
       '[(blank x) (blank x)]     [3 5]       nil

       ;; ordered choice
       '(alt (as x 1) (as y 2))   1           {'x 1}
       '(alt (as x 1) (as y 2))   2           {'y 2}
       '(alt (as x 1) (as y 2))   3           nil

       )

  (are [subject strategy result]
       (= (rewrite subject strategy) result)

       '5    '(rule 5 10)    10

       '5    '(rule (as x 5) (inc x))    6

       '[1 2]   '(rule [(blank x) (blank y)] [y x])    [2 1]

       '2   '(rule (alt 1 2) 3)    3
       '2   '(alt (rule 1 "x") (rule 2 "y"))    "y"

       )

  (->
    (compile '[1 (blank x) [3] (blank x) (alt (as y 1) 2)] 'foo)
    (fipp.clojure/pprint {:width 150})
    )

)
