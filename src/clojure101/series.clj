(ns clojure101.series)

(defn avg [[x y]] (/ (+ x y) 2))

(defn avg-damp
  [seq]
  (map avg (partition 2 1 seq)))

(defn avg-damp-n
  [n]
  (apply comp (repeat n avg-damp)))

(defn sums
  [seq]
  (reductions + seq))

(defn Gregory-Leibniz-n
  [n]
  (/ (int (Math/pow -1 n)) (inc (* 2 n))))

(def Gregory-Leibniz-pi
     (map #(* 4 (Gregory-Leibniz-n %)) (range)))

; π = 3.14159265358979323846264338327950288419716939937510...

(time
 (let [n 100]
   (println n (double (first ((avg-damp-n n) (sums Gregory-Leibniz-pi)))))))
