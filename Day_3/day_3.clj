(ns day-3
  (:require
   [clojure.string]))

(def input-file "input.in")

(defn parser
  [string]
  (->> string
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map #(vector (second %) (nth % 2)))
       (map (fn [s]
              (map #(Integer. %) s)))))

(defn first-half
  []
  (->> input-file
       slurp
       parser
       (map #(reduce * %))
       (reduce +)))

(defn find-muls
  [string]
  (-> string
      (clojure.string/split #"don't\(\)")))

(defn second-half
  []
  (->> input-file
       slurp
       find-muls
       #_parser
       #_(map (fn [s] (reduce * (map #(Integer. %) s))))
       #_(reduce +)))
