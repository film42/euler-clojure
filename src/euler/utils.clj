(ns euler.utils)

;; Using my modexp prime tester: https://gist.github.com/film42/9036065
(defn exp [base pow]
  (reduce * (repeat (bigint pow) (bigint base))))
 
(defn modexp 
  "Modular Exponentiation. No tail optomized loop/recur, but you'll 
   probably never need it."
  [base pow n]
  (if (= pow 0) 1
    (let [z (modexp base (bigint (/ pow 2)) n)]
      (if (even? pow)
        (mod (exp z 2) n)
        (mod (* (exp z 2) base) n)))))
 
(defn rand-bigint
  "Returns a random integer with bitlength n. 
   Credit: http://pastebin.com/KBd862Wr"
  [n] (bigint (new java.math.BigInteger n (new java.util.Random))))
 
(defn uniform-number
  "Return a random number that is between 1 and n-1"
  [n] (+ 1 (rand-bigint (-> (- n 2) str count))))
 
(defn is-prime?
  "Fermat based primality tester"
  ([n] (is-prime? n 200))
  ([n complexity]
    (let [pow (dec n)]
      (loop [k complexity]
        (if (= k 0)
          true
          (let [res (modexp
                      (uniform-number n) pow n)]
            (if-not (= res 1)
              false
              (recur (dec k)))))))))
