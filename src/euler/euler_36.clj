(ns euler.euler-36
  (:require [clojure.string :as s]))

(defn int->binary [n]
  (Integer/toBinaryString n))

(defn pal? [n]
  (= (str n) (s/reverse (str n))))

(defn do-36 []
  (reduce +
          (filter #(and (pal? %) (pal? (int->binary %)))
                  (range 1000000))))

(defn -main [& args]
  (time (println (do-36))))
