(require 'clojure.string)

(def input-file "input.in")

(defn parser
  [string]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" string)
       (map #(vector (second %) (nth % 2)))
       (map (fn [s]
              (map #(Integer. %) s)))))

(defn first-half
  []
  (reduce + (map #(reduce * %) (parser (slurp input-file)))))

(defn find-muls
  [string])

(defn second-half
  []
  (->> (slurp input-file)
       (find-muls)
       (map (fn [s] (reduce * (map #(Integer. %) s))))
       (reduce +)))
