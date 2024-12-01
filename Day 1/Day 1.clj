(require 'clojure.string)

(def file-input "input.in")

(defn parse
  "
  Parser of the input.

  Format:  '77442   88154'
           '71181   76363'
           '49755   69158'

  Result: '([77442 88154] [71181 76363] [49755 69158])'
  "
  [string]
  (map (fn [v]
         (vec (map #(Integer. %) (clojure.string/split v #"   "))))
       (clojure.string/split string #"\n")))

(defn left-right
  "
  Transpose the matrix then Sort each row.

  Input:  ([1 2] [5 4] [3 6])

  Output: ([1 3 5] [2 4 6])
  "
  [lst]
  (map sort
       (apply map vector lst)))

(defn result
  "
  Computes the sum of the absolute differences between corresponding elements 
  in two sequences.

  Input:  ([1 3 5] [2 4 6])

  Output: 1 + 1 + 1 = 3
  "
  [lst]
  (reduce + (map #(abs (- %1 %2)) (first lst) (second lst))))

(defn first-half
  []
  (let [fn (comp result left-right parse slurp)] ;; Function Composition
    (fn file-input)))

(defn similarity
  "
  Computes the frequency of a number (num) in a sorted list.

  Input:  5 [1 1 1 2 2 4 5 5 5 6 6 7 ...]

  Output: [1 1 1 2 2 4 5 5 5] -> [5 5 5] -> 3
  "
  [num lst]
  (count (filter #(= num %) (take-while #(<= % num) lst))))

(defn left-right-sim
  "
  For each element in nums1, computes the frequency it appears in nums2.
  It then removes the elements with frequency 0.

  Input:  ([1 2 3] [1 1 1 2 2 3 3 4 4 ...])

  Output: ([:num 1 :sim 3] [:num 2 :sim 2] [:num 3 :sim 4])
  "
  [[nums1 nums2]]
  (->> nums1
       (map #(zipmap [:num :sim] [% (similarity % nums2)]))
       (filter #(> (:sim %) 0))))

(defn total-sim
  "Computes the total similarity score."
  [sims]
  (reduce #(+ (* (:num %2) (:sim %2)) %1) 0 sims))

(defn second-half
  []
  ;; Function Composition
  (let [fn (comp total-sim left-right-sim left-right parse slurp)]
    (fn file-input)))
