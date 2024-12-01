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

(defn -main
  []
  (result (left-right (parse (slurp file-input)))))
