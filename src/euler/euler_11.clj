(ns euler.euler-11
  (:require [clojure.string :as s]))

(def grid-data
  "Open data from file"
  (slurp "files/11.txt"))

(defn- get-grid
  "Get the matrix from the slurped text. Break on new lines, then split and parse for Ints"
  []
  (let [rows  (s/split-lines grid-data)
        colls (map #(s/split % #" ") rows)]
    (for [c colls]
      (map #(java.lang.Integer/parseInt %) c))))

(def grid (get-grid))

(defn- mnth
  "Get an item within the matrix given some row and col indicies"
  [matrix row col]
  (nth (nth matrix row) col))

(defn- dcount
  "Get the count of possible diagonal slices: count + (count - 1)"
  [matrix] ;; diag count
  (+ (count matrix) (dec (count matrix))))

(defn- col
  "Get a column in the matrix given some col number"
  [matrix col]
  (for [c matrix]
    (nth c col)))

(defn- diag-right
  "Super ugly. This returns a diag slice that descends to the right."
  [matrix slice]
  (if (< slice (count matrix))
    ;; First half (col starts 0)
    (let [start (- (count matrix) (inc slice))]
      (loop [i start
             col 0
             acc '()]
        (if (= (count matrix) i)
          acc
          (recur
           (inc i)
           (inc col)
           (conj acc (mnth matrix i col))))))
    ;; Second half (row starts at 0)
    (let [start (- (inc slice) (count matrix))]
      (loop [i start
             row 0
             acc '()]
        (if (= (count matrix) i)
          acc
          (recur
           (inc i)
           (inc row)
           (conj acc (mnth matrix row i)))))) ))

(defn- diag-left
  "Returns a diag slice that descends to the left. Note, we just flip and use the right diag"
  [matrix slice]
  (let [m (map reverse matrix)]
    (diag-right m slice)))

(defn- process
  "Given any list, vec, etc. Find the max product it contains of 4 adjacent numbers.
This method is used heavily. All subproblems use this function."
  [row]
  (loop [acc row
         sub-result 0]
    (let [curr (reduce * (take 4 acc))
          t (rest acc)]
      (if (empty? acc)
        sub-result
        (recur t (max sub-result curr))))))

(defn max-product
  "A large max reducer. Check the max from each valid direction and return the result."
  [matrix]
  (let [side-max (apply max (map process matrix))

        up-max (apply max (for [i (range (count matrix))]
                            (process (col matrix i))))

        r-diag (apply max (for [i (range (dcount matrix))]
                            (process (diag-right matrix i))))

        l-diag (apply max (for [i (range (dcount matrix))]
                            (process (diag-left matrix i))))]

    ;; This is the end
    (max side-max up-max r-diag l-diag)))


;; RUNNER
(defn -main [& args]
  (println (time (max-product grid))))
