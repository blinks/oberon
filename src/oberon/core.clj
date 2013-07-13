(ns oberon.core
  (:use
    [oberon.time])
  (:require
    [overtone.core :as overtone])
  (:gen-class))

(def reason (overtone/midi-out))

(defmulti play-note :c)

(defmethod play-note :default
  [{p :p v :v :or {v 60} :as note}]
  (if (not (nil? p))
    (overtone/midi-note-on reason p v)))

(defn conduct
  ([es]
   (let [m (overtone/metronome 80)
         es' (timeshift (m) es)
         t (:t (first es'))]
     (overtone/apply-at (m t) conduct [m es'])))
  ([m [e & es]]
   (play-note e)
   (if es
     (let [e' (first es)
           t' (:t e')]
       (overtone/apply-at (m t') conduct [m es])))))

(defn binomial-nth
  [p [opt & opts]]
  (cond
    (nil? opts) opt
    (<= (rand) p) opt
    :else (binomial-nth p opts)))

(defn similar-mod?
  "Return the number of shared pitches between these two chords."
  [m a b]
  (let [note-set (fn [c] (->> c (map #(mod % m)) set))
        ma (note-set a)
        mb (note-set b)
        i (clojure.set/intersection ma mb)]
    (count i)))

(defn not-diminished
  [chord]
  (let [cn (map overtone/nth-interval chord)
        ci (overtone/find-chord cn)]
    (not= :diminished (:chord-type ci))))

(def vocal-range
  {:soprano [:C4 :A5]
   :alto [:G3 :F5]
   :tenor [:C3 :A4]
   :bass [:E2 :E4]
   :midi [:C-1 :G10]})

(defn vocal-field
  ([root mode [low high]]
   (vocal-field (overtone/scale-field root mode) [low high]))
  ([notes [low high]]
   (take-while (partial >= (overtone/note high))
               (drop-while (partial > (overtone/note low))
                           notes))))

(defn root-note
  [root]
  (overtone/note (str (name root) -1)))

(defn base-chord
  [root mode intervals]
  (let [nth-interval (partial overtone/nth-interval mode)
        shift (partial + (root-note root))]
    (map (comp shift nth-interval) intervals)))

(defn arpeggio
  [root mode chord]
  (let [ts (base-chord root mode chord)
        ps (apply concat (iterate #(map (partial + 12) %) ts))]
    (vocal-field ps (:midi vocal-range))))

(defn choices
  [part root mode chord]
  (vocal-field
    (arpeggio root mode chord)
    (vocal-range part)))

;;; Motives

(defn rhythmic-motif
  "Nested durations."
  ([duration] (rhythmic-motif duration (repeatedly 4 #(rand-nth [2 3]))))
  ([duration [r & rs]]
   (let [d (/ duration r)]
     (if (nil? rs) duration
       (for [i (range r)] (rhythmic-motif d rs))))))

(defn melodic-motif
  "Generate infinite pitch directions."
  []
  (cons (rand-nth [+1 +1 +1 0 -1 -1 -1])
        (lazy-seq (melodic-motif))))

(defn harmonic-motif
  "Generate infinite chord progressions, avoiding diminished chords."
  ([] (harmonic-motif [0 2 4]))
  ([seed-chord]
   (let [opts (for [i (range 8)] [i (+ i 2) (+ i 4)])
         ndim (filter not-diminished opts)
         near (group-by (partial similar-mod? 7 seed-chord) ndim)
         candidates (apply concat (for [i [1 2]] (near i)))
         chosen (binomial-nth 1/3 candidates)]
     (cons seed-chord (lazy-seq (harmonic-motif chosen))))))

(defn compose-harmony
  ([part root mode rhythm melody harmony]
   (compose-harmony part root mode nil rhythm melody harmony))
  ([pt rt md p' [r & rs] [m & ms] [h & hs]]
   (let [*ps (choices pt rt md h)
         ps (if (nil? p') (shuffle *ps)
              (case m
                0  [p']
                -1 (reverse (take-while #(< % p') *ps))
                +1 (drop-while #(<= % p') *ps)))
         pitch (binomial-nth 1/4 ps)
         duration (seq+ r)
         velocity 70
         e {:d duration :p pitch :v velocity :x pt}]
     (cons e (if rs (lazy-seq
                      (compose-harmony pt rt md pitch rs ms hs)))))))

(defn harmonic-order
  [hx]
  (map #(+ (nth hx 1) %) [0 4 2 6 3 1 5]))

(defn compose-cell
  [pt rt md hh rx mx h]
  (let [*ps (choices pt rt md h)
        pitch (rand-nth *ps)
        [r & rs] (concat+ rx)
        duration r
        velocity 90
        init {:d duration :p pitch :v velocity :x pt}
        ; For the rest of this melody, use the entire scale.
        scale (repeat (apply sorted-set (take hh (harmonic-order h))))]
    (cons init (lazy-seq (compose-harmony
                           pt rt md pitch rs mx scale)))))

(defn compose-melody
  [pt rt md hh rx mx hx]
  (mapcat #(compose-cell pt rt md hh %1 mx %2) rx hx))

(defn motif->theme
  [root mode rhythm melody harmony]
  (let [ch #(compose-harmony % root mode rhythm melody harmony)
        cm #(compose-melody %1 root mode %2 rhythm melody harmony)]
  (-> (relative->absolute (cm :bass 3))
      (with (relative->absolute (cm :tenor 4)))
      (with (relative->absolute (cm :alto 5)))
      (with (relative->absolute (cm :soprano 6))))))

(defn -main
  "Compose and perform a fantasia."
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (let [r' (rhythmic-motif 8)
        rx (concat r' r')
        mx (melodic-motif)
        hx (harmonic-motif)
        mt (motif->theme :C :minor rx mx hx)]
    (->> mt conduct)))
