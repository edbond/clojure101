(ns clojure101.w1e4
  (:use [criterium.core]))

; Phi = (1 + sqrt(5)) / 2
(def phi (/ (+ 1 (Math/sqrt 5)) 2))

; Fib(n) = round(Phi^n / sqrt(5))
(defn nth-fib
  [n]
  (bigint (/ (Math/pow phi n) (Math/sqrt 5))))

(def memoize-nth-fib (memoize nth-fib))

(defn bench1
  []
  (do
    (println "Non-memoize")
    (with-progress-reporting
      (bench (doall (map nth-fib (range 10 100))) :verbose))
    (println "Memoizing")
    (with-progress-reporting
      (bench (doall (map memoize-nth-fib (range 10 100))) :verbose))))

(time (dotimes [i 100000] (doall (map nth-fib (range 10 100)))))
(time (dotimes [i 100000] (doall (map memoize-nth-fib (range 10 100)))))
