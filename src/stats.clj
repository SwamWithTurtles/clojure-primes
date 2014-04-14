(ns clojure_primes.stats-runner)

(defn expt [base exp]
  (loop [base base exp exp acc 1]
    (if (zero? exp) acc
                    (recur base (- exp 1) (* acc base)))))

(defn det-prime? [n]

  (zero? (count (filter #(zero? (rem n %)) (concat [2] (range 3 n 2))))))

(defn powerMod [b e m]
  (defn m* [p q] (mod (* p q) m))
  (loop [b b, e e, x 1]
    (if (zero? e) x
                  (if (even? e) (recur (m* b b) (/ e 2) x)
                                (recur (m* b b) (quot e 2) (m* b x))))))

(defn is-witness-for? [w, p]
  (= 1 (powerMod w (- p 1) p)))

(defn nondet-prime? [n noAttempts]
  (loop [num n recursLeft noAttempts]
    (if (zero? recursLeft) true ; Probably not prime
                           (if (is-witness-for? (+ 1 (rand-int (- n 1))) n) (recur n (- recursLeft 1))
                                                                            false ;Composite
                                                                             ))))

(defn rdmOfMagnitude [order-of-magnitude]
  (+ (rand-int (expt 10 order-of-magnitude)) 4))

(defn algorithms-agree [n recur-attempts]
  (= (det-prime? n) (nondet-prime? n recur-attempts)))

(defn run-statistics [order-of-magnitude recur-attempts sample-size]
  (loop [num-correct 0 total-num 0 samples (take sample-size (repeatedly #(rdmOfMagnitude order-of-magnitude)))]
    (if (empty? samples) [num-correct total-num]
                         (recur
                                (+ num-correct (if (algorithms-agree (first samples) recur-attempts) 1 0))
                                (+ total-num 1)
                                (rest samples)))))