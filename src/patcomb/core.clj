(ns patcomb.core
  (:refer-clojure :exclude [compile]))

(defn head [x]
  (if (seq? x)
    (first x)
    (class x)))

(defmulti -match (fn [form env subject] (head form)))

(defmulti -proc (fn [form subject] (head form)))

(defmethod -match 'const
  [[_ x] env subject]
  (assert env)
  (when (= x subject)
    env))

(defmethod -proc 'const
  [[_ x] subject]
  [[:test `(= ~x ~subject)]])

(defmethod -match 'blank
  [[_ sym] env subject]
  (assert env)
  (assert (symbol? sym))
  (if-let [x (env sym)]
    (when (= x subject)
      env)
    (assoc env sym subject)))

(defmethod -proc 'blank
  [[_ sym] subject]
  [[:bind sym subject]])

(defmethod -match 'as
  [[_ sym subpat] env subject]
  (assert (symbol? sym))
  (assert env)
  (when-let [env* (-match subpat env subject)]
    (assoc env* sym subject)))

(defmethod -proc 'as
  [[_ sym subpat] subject]
  (concat (-proc (list 'blank sym) subject)
          (-proc subpat subject)))

(defmethod -match java.lang.Long
  [value env subject]
  (-match (list 'const value) env subject))

(defmethod -proc java.lang.Long
  [value subject]
  (-proc (list 'const value) subject))

(defmethod -match clojure.lang.PersistentVector
  [subpats env subject]
  (assert env)
  (when (and (vector? subject) (= (count subpats) (count subject)))
    (reduce (fn [env [pattern subject]]
              (-match pattern env subject))
            env
            (map vector subpats subject))))

(defmethod -proc clojure.lang.PersistentVector
  [subpats subject]
  (let [n (count subpats)]
    (concat [[:test `(vector? ~subject)]
             [:test `(= ~n (count ~subject))]]
            (for [[i subpat] (map vector (range n) subpats)
                  :let [sym (symbol (str subject "__" i))]
                  foo (cons [:bind sym `(nth ~subject ~i)]
                            (-proc subpat sym))]
              foo))))

;TODO some kind of generic traversal/zip thing for matching composites
;;TODO -proc

(defn match [pattern subject]
  (-match pattern {} subject))

(defn compile [pattern subject]
  (let [proc (-proc pattern '__subject)
        {:keys [k names]}
        (reduce (fn [state [op & args]]
                  (case op
                    :bind
                    (let [[sym init] args]
                      (if-let [existing (get-in state [:names sym])]
                        (update-in state [:k] conj (fn [x]
                                                     `(when (= ~existing ~init)
                                                        ~x)))
                        (-> state
                           (update-in [:names] conj sym)
                           (update-in [:k] conj (fn [x]
                                                  `(let [~sym ~init]
                                                     ~x))))))
                    :test
                    (let [[expr] args]
                      (update-in state [:k] conj (fn [x]
                                                   `(when ~expr
                                                      ~x))))
                    ))
                {:k []
                 :names #{}}
                proc)
        expr (reduce (fn [x f]
                       (f x))
                     (into {} (for [sym names
                                    :when (not (re-find #"^__" (str sym)))]
                                [(list 'quote sym) sym]))
                     (reverse k))]
    `(let [~'__subject ~subject]
       ~expr)))

(defn run [pattern subject]
  (eval (compile pattern subject)))

(comment

  (require '[clojure.test :refer [is are]])

  (are [pattern subject substitutions]
       (= (match pattern subject) (run pattern subject) substitutions)

       '(const 5)                 5           {}
       '5                         5           {}
       '(const 5)                 0           nil
       '(blank x)                 5           {'x 5}
       '(as x (const 1))          1           {'x 1}
       '[]                        []          {}
       '[(blank x) 10]            [5 10]      {'x 5}
       '[(blank x) 10]            [5 11]      nil
       '[(blank x) (blank x)]     [3 3]       {'x 3}
       '[(blank x) (blank x)]     [3 5]       nil

       )

  (->
    (compile '[1 (blank x) [3] (blank x) 1] 'foo)
    (fipp.clojure/pprint {:width 150})
    )

)
