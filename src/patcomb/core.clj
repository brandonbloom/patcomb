(ns patcomb.core
  (:refer-clojure :exclude [compile]))

(defn head [x]
  (if (seq? x)
    (first x)
    (class x)))

(def ^:dynamic eq)
(def ^:dynamic isvec)
(def ^:dynamic cnt)
(def ^:dynamic check)
(def ^:dynamic *env*)
(def ^:dynamic fail)

(defmulti match! (fn [form subject] (head form)))

(defmethod match! 'const
  [[_ x] subject]
  (check (eq x subject)))

(defmethod match! 'blank
  [[_ sym] subject]
  (assert (symbol? sym))
  (if-let [x (*env* sym)]
    (check (eq x subject))
    (set! *env* (assoc *env* sym subject))))

(defmethod match! 'as
  [[_ sym subpat] subject]
  (assert (symbol? sym))
  (set! *env* (assoc *env* sym subject))
  (match! subpat subject))

(defmethod match! java.lang.Long
  [value subject]
  (match! (list 'const value) subject))

(defmethod match! clojure.lang.PersistentVector
  [subpats subject]
  (check (isvec subject)) ;TODO abstract
  (check (eq (cnt subpats) (cnt subject)))
  (doseq [[pattern subject] (map vector subpats subject)]
    (match! pattern subject)))

(def failed (ex-info "match failed" {}))

(defn match* [pattern subject]
  (binding [*env* *env*]
    (try
      (match! pattern subject)
      *env*
      (catch clojure.lang.ExceptionInfo e
        (when-not (identical? e failed)
          (throw))))))

(defmethod match! 'alt
  [[_ & subpats] subject]
  (set! *env* (some #(match* % subject) subpats)))

(defn match [pattern subject]
  (binding [eq =
            isvec vector?
            cnt count
            check #(when-not % (fail))
            *env* {}
            fail #(throw failed)]
    (try
      (match! pattern subject)
      *env*
      (catch clojure.lang.ExceptionInfo e
        (when-not (identical? e failed)
          (throw))))))

(defn proc->clj [proc names]
  (let [{:keys [k names]}
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
                    :alt
                    (update-in state [:k] conj (fn [x]
                                                 ;;XXX This doesn't use the x arg, but still works because the
                                                 ; recursion supplies the names map construction code. Will
                                                 ; break when more interesting success code is wanted.
                                                 `(or ~@(map #(proc->clj % (:names state)) args))))
                    ))
                {:k []
                 :names names}
                proc)]
    (reduce (fn [x f]
              (f x))
            (into {} (for [sym names
                           :when (not (re-find #"^__" (str sym)))]
                       [(list 'quote sym) sym]))
            (reverse k))))

;(defn compile
;  [pattern subject]
;  (let [proc (-proc pattern '__subject)
;        expr (proc->clj proc #{})]
;    `(let [~'__subject ~subject]
;       ~expr)))
;
;(defn run [pattern subject]
;  (eval (compile pattern subject)))

(comment

  (require '[clojure.test :refer [is are]])

  (are [pattern subject substitutions]
       (= (match pattern subject) #_(run pattern subject) substitutions)

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

  (->
    (compile '[1 (blank x) [3] (blank x) (alt (as y 1) 2)] 'foo)
    (fipp.clojure/pprint {:width 150})
    )

)
