(ns patcomb.core)

(defprotocol Matcher
  (-match [this env subject]))

(defn const [x]
  (reify Matcher
    (-match [_ env subject]
      (assert env)
      (when (= x subject)
        env))))

(defn blank [sym]
  (assert (symbol? sym))
  (reify Matcher
    (-match [_ env subject]
      (assert env)
      (if-let [x (env sym)]
        (when (= x subject)
          env)
        (assoc env sym subject)))))

(defn as [sym matcher]
  (assert (symbol? sym))
  (assert (satisfies? Matcher matcher))
  (reify Matcher
    (-match [_ env subject]
      (assert env)
      (when-let [env* (-match matcher env subject)]
        (assoc env* sym subject)))))

(defn pattern [matcher]
  (assert (satisfies? Matcher matcher))
  (reify Matcher
    (-match [_ env subject]
      (if env
        (and (-match matcher {} subject) {})
        (-match matcher {} subject)))))

(defn match [pattern subject]
  (-match pattern nil subject))

(comment

  (require '[clojure.test :refer [is]])

  (is (= (match (pattern (const 5)) 5) {}))
  (is (= (match (pattern (const 5)) 0) nil))
  (is (= (match (pattern (blank 'x)) 5) {'x 5}))
  (is (= (match (pattern (pattern (blank 'x))) 5) {}))
  (is (= (match (pattern (as 'x (const 1))) 1) {'x 1}))

)
