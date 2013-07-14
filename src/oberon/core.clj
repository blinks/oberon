(ns oberon.core
  (:use
    [oberon.time])
  (:require
    [overtone.core :as overtone])
  (:gen-class))

(def reason (overtone/midi-out))
(def tap (overtone/metronome 100))

(defmulti play-note :x)

(defn play-note-on-channel
  [ch {:keys [t d p v] :or {v 60}}]
  (overtone/after-delay
    (rand-int 50)
    (fn []
      (overtone/midi-note-on reason p v ch)
      (overtone/after-delay  ; use a minimum note length
        (+ (max 200 (overtone/beat-ms d (overtone/metro-bpm tap)))
           (rand-int 50))
        (fn []
          (overtone/midi-note-off reason p ch))))))

(defmethod play-note :default
  [{p :p :as note}]
  (if (not (nil? p))
    (play-note-on-channel 0 note)))

(defmethod play-note :alto
  [{p :p :as note}]
  (if (not (nil? p))
    (let [n (assoc note :v (* 4/5 (:v note)))]
      (play-note-on-channel 1 note))))

(defmethod play-note :tenor
  [{p :p :as note}]
  (if (not (nil? p))
    (let [n (assoc note :v (* 3/4 (:v note)))]
      (play-note-on-channel 2 note))))

(defmethod play-note :bass
  [{p :p :as note}]
  (if (not (nil? p))
    (let [n (assoc note :v (* 2/3 (:v note)))]
      (play-note-on-channel 3 note))))

(defn conduct
  ([es]
   (let [m tap
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
  ([]
   (let [r (rhythmic-motif 8 (cons 2 (repeatedly 3 #(rand-nth [2 3]))))]
     (concat r r)))
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
   (let [opts (for [i [0 2 3 4 5]] [i (+ i 2) (+ i 4)])
         near (group-by (partial similar-mod? 7 seed-chord) opts)
         candidates (apply concat (for [i [1 2]] (near i)))
         chosen (binomial-nth 1/3 candidates)]
     (cons seed-chord (lazy-seq (harmonic-motif chosen))))))

(defn compose-harmony
  ([part root mode rhythm melody harmony]
   (compose-harmony part root mode nil rhythm melody harmony))
  ([pt rt md p' [r & rs] [m & ms] [h & hs]]
   ; TODO: Add a flourish by subdividing duration?
   (let [*ps (choices pt rt md h)
         ps (if (nil? p') (shuffle *ps)
              (case m
                0  [p' nil]
                -1 (reverse (take-while #(< % p') *ps))
                +1 (drop-while #(<= % p') *ps)))
         pitch (binomial-nth 1/3 (conj ps nil))
         duration (seq+ r)
         velocity (rand-nth (range 50 80))
         e {:d duration :p pitch :v velocity :x pt}]
     (cons e (if rs (lazy-seq
                      (compose-harmony pt rt md pitch rs ms hs)))))))

(defn harmonic-order
  [hx]
  (map #(+ (nth hx 1) %) [0 4 2 3 5 1 6]))

(defn compose-cell
  [pt rt md hh rx mx h]
  (let [*ps (choices pt rt md h)
        pitch (rand-nth *ps)
        [r & rs] (concat+ rx)
        duration r
        velocity (rand-nth [0 0 60 70 75 80])
        init {:d duration
              :p (if (zero? velocity) nil pitch)
              :v velocity
              :x pt}
        ; For the rest of this melody, use the entire scale.
        scale (repeat (apply sorted-set (take hh (harmonic-order h))))]
    (cons init (lazy-seq (compose-harmony
                           pt rt md pitch rs mx scale)))))

(defn compose-melody
  [pt rt md hh rx mx hx]
  (mapcat #(compose-cell pt rt md hh %1 mx %2) rx hx))

(defn motif->theme
  [root mode rhythm melody harmony]
  (let [cm #(compose-melody %1 root mode %2 rhythm melody harmony)]
  (-> (relative->absolute (cm :bass 2))
      (with (relative->absolute (cm :tenor 3)))
      (with (relative->absolute (cm :alto 4)))
      (with (relative->absolute (cm :soprano 5))))))

(defn sonata
  [depth rt md rx mx hx]
  (if (<= depth 0) []
    (let [shift (count rx)
          progression (take shift hx)
          hx' (reverse (take shift hx))
          t (base-chord rt md (nth hx (dec shift)))
          {tr :root tm :chord-type} (overtone/find-chord t)]
      ;(printf "Progression: %s\n"
      ;        (apply str (map #(let [base (base-chord rt md %)
      ;                               {:keys [root chord-type]}
      ;                               (overtone/find-chord base)]
      ;                           (str (name root) (name chord-type)))
      ;                        (take shift hx))))
      ;(printf "Modulate to %s %s\n" tr tm)
      (->> (motif->theme rt md rx mx hx)
           (then (motif->theme rt md rx mx hx))
           (then (sonata (dec depth) tr tm rx mx hx))
           (then (motif->theme rt md rx mx hx'))
           (then (motif->theme rt md rx mx hx))
           (then (sonata (dec depth) tr tm rx mx hx))
           (then (motif->theme rt md rx mx hx'))))))

(defn measure
  [hx]
  (case (count (first hx))
    2 (case (count (ffirst hx)) 2 "4/4" 3 "6/8")
    3 "3/4"))

(defn -main
  "Compose and perform a sonata."
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (let [root (rand-nth (vals overtone/REVERSE-NOTES))
        mode (rand-nth [:major :minor])
        rx (rhythmic-motif)
        mx (melodic-motif)
        hx (harmonic-motif)
        piece (sonata 5 root mode rx mx hx)]
    (printf "Sonata in %s %s; %s\n" root mode (measure hx))
    (->> piece conduct)))
