(require 'clojure.string)

(def file-input "input.in")

(defn parser
  "
  Parser of the input.

  Format: '22' '25' '27' '28' '30' '31' '32' '29'
          '72' '74' '75' '77' '80' '81' '81'
          '52' '53' '55' '58' '59' '63'

  Result: ([22 25 27 28 30 31 32 29] [72 74 75 77 80 81 81] [52 53 55 58 59 63])
  "
  [string]
  (map (fn [v]
         (vec (map #(Integer. %) (clojure.string/split v #" "))))
       (clojure.string/split string #"\n")))

(defn comp-seq
  ([lst] (comp-seq lst <))
  ([[n1 & rst] op]
   (if (empty? rst)
     true
     (if (op n1 (first rst))
       (recur rst op) ;; Tail Call Optimization
       false))))      ;; Short-Circuit

(defn inc-or-dec
  [lst]
  (filter #(or (comp-seq %) (comp-seq % >)) lst))

(defn comp-diff
  [lst n]
  (every? #(<= (abs (- (first %) (second %))) n)
          (partition 2 1 lst)))

(defn diff<=3
  [lst]
  (filter #(comp-diff % 3) lst))

(defn first-half
  []
  (let [fn (comp count diff<=3 inc-or-dec parser slurp)]
    (fn file-input)))

(defn dampener [lsts]
  (filter
   (fn [lst]
     (let [len (count lst)]
       (some
        (fn [i]
          (let [rem (concat (subvec lst 0 i) (subvec lst (inc i) len))]
            (and (or (comp-seq rem) (comp-seq rem >)) (comp-diff rem 3))))
        (range len))))
   lsts))

(defn second-half
  []
  (let [fn (comp count dampener parser slurp)]
    (fn file-input)))
