(ns day-1
  (:require
   [clojure.string]))

(def file-input "input.in")

(defn parser
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
  (map sort                     ;; Sort
       (apply map vector lst))) ;; Transpose

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
  (let [fn (comp result left-right parser slurp)] ;; Function Composition
    (fn file-input)))

(defn similarity
  "
  Computes the frequency of a number (num) in a sorted list.

  Input:  5 [1 1 1 2 2 4 5 5 5 6 6 7 ...]

  Output: [1 1 1 2 2 4 5 5 5] -> [5 5 5] -> 3
  "
  [num lst]
  (count (filter #(= num %) (take-while #(<= % num) lst))))

(defn similarities
  "
  For each element in 'left', computes the frequency it appears in 'right'.
  It then removes the elements with frequency 0.

  Input:  ([1 2 3] [1 1 1 2 2 3 3 4 4 ...])

  Output: ([:num 1 :sim 3] [:num 2 :sim 2] [:num 3 :sim 4])
  "
  [[left right]]
  (->> left
       (map #(zipmap [:num :sim] [% (similarity % right)]))
       (filter #(> (:sim %) 0))))

(defn total-similarity
  "Computes the total similarity score."
  [sims]
  (reduce #(+ (* (:num %2) (:sim %2)) %1) 0 sims))

(defn second-half
  []
  ;; Function Composition
  (let [fn (comp total-similarity similarities left-right parser slurp)]
    (fn file-input)))
