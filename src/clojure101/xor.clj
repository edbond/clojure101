(ns xor 
  (:use clojure.test))

(defmacro xor
  "Evaluates exprs one at a time, from left to right.  If only one form returns
  a logical true value (neither nil nor false), returns true.  If more than one
  value returns logical true or no value returns logical true, retuns a logical
  false value.  As soon as two logically true forms are encountered, no
  remaining expression is evaluated.  (xor) returns nil."
  ([] nil)
  ([f & r]
     `(loop [t# false f# '[~f ~@r]]
        (if-not (seq f#) t#
                (let [fv# (eval (first f#))]
                  (cond
                   (and t# fv#) false
                   (and (not t#) fv#) (recur true (rest f#))
                   :else (recur t# (rest f#))))))))

(def test-output (ref []))
(defn- doit
  "Returns val and creates the side effect of printing num."
  [num val]
  (dosync
   (alter test-output conj num))
  val)

(defn get-test-output
  "Returns test-output content and clears it"
  []
  (let [t @test-output]
    (dosync (ref-set test-output []))
    t))

(deftest nullary
  (is (not (xor))))

(deftest unary
  (is (get-test-output))
  (is (xor (doit 1 true)))
  (is (= [1] (get-test-output)))
  (is (not (xor (doit 1 nil))))
  (is (= [1] (get-test-output))))

(deftest binary  
  (is (not (xor (doit 1 nil) (doit 2 nil))))
  (is (= [1 2] (get-test-output)))
  (is (not (xor (doit 1 true) (doit 2 true))))
  (is (= [1 2] (get-test-output)))
  (is (xor (doit 1 nil) (doit 2 true)))
  (is (= [1 2] (get-test-output)))
  (is (xor (doit 1 true) (doit 2 nil)))
  (is (= [1 2] (get-test-output))))

(deftest ternary
  (is (not (xor (doit 1 nil) (doit 2 nil) (doit 3 nil))))
  (is (= [1 2 3] (get-test-output)))
  (is (xor (doit 1 nil) (doit 2 nil) (doit 3 true)))
  (is (= [1 2 3] (get-test-output)))
  (is (xor (doit 1 nil) (doit 2 true) (doit 3 nil)))
  (is (= [1 2 3] (get-test-output)))
  (is (not (xor (doit 1 nil) (doit 2 true) (doit 3 true))))
  (is (= [1 2 3] (get-test-output)))
  (is (xor (doit 1 true) (doit 2 nil) (doit 3 nil)))
  (is (= [1 2 3] (get-test-output)))
  (is (not (xor (doit 1 true) (doit 2 nil) (doit 3 true))))
  (is (= [1 2 3] (get-test-output)))
  (is (not (xor (doit 1 true) (doit 2 true) (doit 3 nil))))
  (is (= [1 2] (get-test-output)))
  (is (not (xor (doit 1 true) (doit 2 true) (doit 3 true))))
  (is (= [1 2] (get-test-output))))

(deftest n-ary  
  (is (not (xor (doit 1 nil) (doit 2 nil) (doit 3 nil)
                (doit 4 nil) (doit 5 nil) (doit 6 nil))))
  (is (= [1 2 3 4 5 6] (get-test-output)))
  (is (not (xor (doit 1 nil) (doit 2 true) (doit 3 nil)
                (doit 4 true) (doit 5 nil) (doit 6 nil))))
  (is (= [1 2 3 4] (get-test-output)))
  (is (xor (doit 1 true) (doit 2 nil) (doit 3 nil)
           (doit 4 nil) (doit 5 nil) (doit 6 nil)))
  (is (= [1 2 3 4 5 6] (get-test-output)))
  (is (not (xor (doit 1 true) (doit 2 true) (doit 3 true)
                (doit 4 true) (doit 5 true) (doit 6 true))))
  (is (= [1 2] (get-test-output)))
  (is (not (xor (doit 1 nil) (doit 2 nil) (doit 3 nil)
                (doit 4 nil) (doit 5 true) (doit 6 true))))
  (is (= [1 2 3 4 5 6] (get-test-output))))


(run-tests)
