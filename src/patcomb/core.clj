(ns patcomb.core)

(defmulti -match (fn [form env subject]
                   (if (seq? form)
                     (first form)
                     (class form))))

(defmethod -match 'const
  [[_ x] env subject]
  (assert env)
  (when (= x subject)
    env))

(defmethod -match 'blank
  [[_ sym] env subject]
  (assert env)
  (assert (symbol? sym))
  (if-let [x (env sym)]
    (when (= x subject)
      env)
    (assoc env sym subject)))

(defmethod -match 'as
  [[_ sym subpat] env subject]
  (assert (symbol? sym))
  (assert env)
  (when-let [env* (-match subpat env subject)]
    (assoc env* sym subject)))

(defmethod -match java.lang.Long
  [value env subject]
  (-match (list 'const value) env subject))

(defmethod -match clojure.lang.PersistentVector
  [subpats env subject]
  (assert env)
  (when (and (vector? subject) (= (count subpats) (count subject)))
    (reduce (fn [env [pattern subject]]
              (-match pattern env subject))
            env
            (map vector subpats subject))))

;TODO some kind of generic traversal/zip thing for matching composites
;;TODO -compile

(defn match [pattern subject]
  (-match pattern {} subject))

(comment

  (require '[clojure.test :refer [is]])

  (is (= (match '(const 5) 5) {}))
  (is (= (match '(const 5) 0) nil))
  (is (= (match '(blank x) 5) {'x 5}))
  (is (= (match '(as x (const 1)) 1) {'x 1}))
  (is (= (match '[] []) {}))
  (is (= (match '[(blank x) 10] [5 10]) {'x 5}))
  (is (= (match '[(blank x) 10] [5 11]) nil))

)
