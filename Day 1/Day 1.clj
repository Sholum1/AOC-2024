(require 'clojure.string)

(def file-input "input.in")

(defn parse
  [string]
  (map (fn [v]
         (vec (map #(Integer. %) (clojure.string/split v #"   "))))
       (clojure.string/split string #"\n")))

(defn left-right
  [lst]
  (map sort
       (apply map vector lst)))

(defn result
  [lst]
  (reduce + (map #(abs (- %1 %2)) (first lst) (second lst))))

(defn first-half
  []
  (let [fn (comp result left-right parse slurp)]
    (fn file-input)))

(defn similarity
  [num lst]
  (count (filter #(= num %) (take-while #(<= % num) lst))))

(defn left-right-sim
  [[nums1 nums2]]
  (->> nums1
       (map #(vector % (similarity % nums2)))
       (filter #(> (second %) 0))))

(defn total-sim
  [sims]
  (reduce (fn [acc [num sim]] (+ (* num sim) acc)) 0 sims))

(defn second-half
  []
  (let [fn (comp total-sim left-right-sim left-right parse slurp)]
    (fn file-input)))
