(ns euler.euler-18
  (:require [clojure.string :as s]))

(defprotocol INode
  (parent [this])
  (distance [this])
  (value [this])
  (total [this])
  (nrow [this])
  (ncol [this]))

(deftype Node [p d r c v t]
  INode
  (parent [this] p)
  (distance [this] d)
  (value [this] v)
  (total [this] t)
  (nrow [this] r)
  (ncol [this] c))

(defprotocol IRevPQ
  (enq [this node])
  (deq [this])
  (size [this])
  (empty?? [this]))

(deftype RevPQ [state]
  IRevPQ
  (enq [this node]
    ;;(println "Adding...")
    (swap! state conj node)
    ;; Pretty crappy, but works
    (reset! state (sort-by distance @state)))
  (deq [this]
    (let [fst (first @state)
          ;; Get all same distance to eval
          fil (filter #(= (distance fst) (distance %)) @state)
          ;; Sort the filtered list by greatest total
          srt (sort-by #(unchecked-negate (total %)) fil)
          n (first srt)]
      (doseq [s srt] (println "V: " (value s) (total s )))
      (println "---")
      (reset! state (sort-by distance (rest srt)))
      n))
  (size [this] (count @state))
  (empty?? [this]
    (zero? (size this))))


(defn- get-grid
  "Get the matrix from the slurped text. Break on new lines, then split/ parse ints"
  []
  (let [rows  (s/split-lines
                (slurp "files/18.txt"))
        colls (map #(s/split % #" ") rows)]
    (for [c colls]
      (map #(java.lang.Integer/parseInt %) c))))

(def grid (get-grid))

(def cache (atom {}))

(defn- add-cache [row col]
  (swap! cache conj {(symbol (str row col)) true}))

(defn- cached? [row col]
  (let [s (@cache (symbol (str row col)))]
    (if (nil? s)
      false true)))


(defn- mnth
  "Get an item within the matrix given some row and col indicies"
  [matrix row col]
  ;;(println row col)
  (nth (nth matrix row) col))

(defn- child-for-nth
  "node being the parent node, and r/c being the child coords"
  [node r c]
  ;;(println "C-4-N:" r c (= r (count grid)))
  (if (= r (count grid))
    nil
    (let [v (mnth grid r c)]
      (if (nil? v)
        nil
        (->Node node (inc (distance node)) r c v (+ (total node) v))))))

(defn- children
  "Get the children of a node in the tree-matrix"
  [node]
  ;;(println "Child:" (nrow node) (inc (nrow node)))
  (let [r (nrow node)
        c (ncol node)
        ch1 (child-for-nth node (inc r) c)
        ch2 (child-for-nth node (inc r) (inc c))]
    (cond
     (and (nil? ch1) (nil? ch2)) []
     (and (not (nil? ch1)) (nil? ch2)) [ch1]
     (and (nil? ch1) (not (nil? ch2))) [ch2]
     :else [ch1 ch2])))

(defn- print-back [node]
  (loop [n node]
    (when-not (nil? n)
      (print (value n) "->")
      (recur (parent n))))
  (println))

(defn- get-max-path []
  (let [pq (->RevPQ (atom '()))]
    ;; 1) Add first node
    (enq pq (->Node nil 0 0 0 (mnth grid 0 0) (mnth grid 0 0)))
    (add-cache 0 0)
    ;; 2) Run dijkstra's
    (loop []
      ;;(println "Stepping...")
      (let [node (deq pq)
            ch (children node)]

        ;; Enqueue all non cached children
        (doseq [c ch]
          (when-not (cached? (nrow c) (ncol c))
            (add-cache (nrow c) (ncol c))
            (enq pq c)))

        ;;(println "Children: " (count ch))
        ;;(println "Queue:" (size pq))
        
        (if (or (empty? ch) (empty?? pq))
          (do
            (while (not (empty?? pq))
              (print-back (deq pq)))
            node)
       
          (recur))))))

;;
;; Attempt Two
;;;;;;;;;;;;;;;;;;;;;;


(def state (atom (vec grid)))

(defn- mreplace [matrix row col value]
  (let [r (assoc (vec (nth matrix row)) col value)]
    (reset! state (assoc (vec matrix) row r))))

(defn- max-for-pt [matrix row col]
  (let [value (mnth matrix row col)]
    (max
     (+ value (mnth matrix (inc row) col))
     (+ value (mnth matrix (inc row) (inc col))))))

(defn- do-18 []
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
  (time (println (do-18))))
