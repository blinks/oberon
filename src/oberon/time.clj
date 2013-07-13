(ns oberon.time
  (:require
    [overtone.core :as overtone]))

(defn timeshift
  [dt es]
  (map (fn [{:keys [t] :as e}] (assoc e :t (+ dt t))) es))

(defn before? [a b] (<= (:t a) (:t b)))

(defn with
  [[a & as :as aas] [b & bs :as bbs]]
  (cond
    (empty? aas) bbs
    (empty? bbs) aas
    (before? a b)
      (cons a (lazy-seq (with as bbs)))
    :otherwise
      (cons b (lazy-seq (with aas bs)))))

(defn seq+
  "Return the total of all nested numbers."
  [xs]
  (cond
    (number? xs) xs
    (seq? xs) (apply + (flatten xs))
    :else 0))

(defn concat+
  "Return a flat sequence."
  [xs]
  (cond
    (number? xs) [xs]
    (seq? xs) (if (< (rand) 1/3)
                (apply concat (map concat+ xs))
                [(seq+ xs)])))

(defn relative->absolute
  "Convert times from :d to :t."
  ([es] (relative->absolute 0 es))
  ([t [e & es]]
   (cons (assoc e :t t)
         (if (nil? es) nil
           (lazy-seq (relative->absolute (+ t (:d e)) es))))))

(defn duration
  [notes]
  (let [{:keys {t d}} (last notes)]
    (+ t d)))

(defn then
  ([limit later earlier]
   (->> earlier
       (with (timeshift limit later))))
  ([later earlier]
   (then (duration earlier) later earlier)))
