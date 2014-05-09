;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;; Find the sum of all the primes below two million.
(ns euler.euler-12
  (:use [euler.utils]))

(defn -main [& args]
  (reduce +
          (filter is-prime? (range 1 2000000))))
