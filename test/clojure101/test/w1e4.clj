(ns clojure101.test.w1e4
  (:use [clojure.test]
        [clojure101.w1e4]))

(deftest nth-fib-test
  (is (=
    (map nth-fib [10 20 30 40 50 60 70])
         '(55 6765 832040 102334155 12586269025 1548008755920 190392490709135))))
