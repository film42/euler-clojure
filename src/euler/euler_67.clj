(ns euler.euler-67
  (:require [clojure.string :as s]))

(defn- mnth
  "Get an item within the matrix given some row and col indicies"
  [matrix row col]
  ;;(println row col)
  (nth (nth matrix row) col))

(defn- get-grid
  "Get the matrix from the slurped text. Break on new lines, then split/ parse ints"
  []
  (let [rows  (s/split-lines
                (slurp "files/67.txt"))
        colls (map #(s/split % #" ") rows)]
    (for [c colls]
      (map #(java.lang.Integer/parseInt %) c))))

(def state (atom (vec (get-grid))))

(defn- mreplace [matrix row col value]
  (let [r (assoc (vec (nth matrix row)) col value)]
    (reset! state (assoc (vec matrix) row r))))

(defn- max-for-pt [matrix row col]
  (let [value (mnth matrix row col)]
    (max
     (+ value (mnth matrix (inc row) col))
     (+ value (mnth matrix (inc row) (inc col))))))

(defn- do-67 []
  (loop [i (- (count @state ) 2)]
    (let [r (nth @state i)]
      
      (doseq [j (range (count r))]
        (mreplace @state i j
                  (max-for-pt @state i j)))
      
      (if (zero? i)
        (mnth @state 0 0)
        (recur (dec i))))))

;; Runner
(defn -main [& args]
  (time (println (do-67))))
