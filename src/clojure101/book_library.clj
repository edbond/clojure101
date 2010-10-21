(ns clojure101.book_library
  (:use clojure.test))

(defprotocol Library
  (empty? [this] "Returns true if library is empty")
  (content [this] "Get content of library")
  (add-book [this book] "Adds book to library")
  (remove-book [this book] "Remove book from library"))

(defrecord RefLibrary [#^clojure.lang.Ref shelf]
  Library
  (empty? [this] (clojure.core/empty? @shelf))
  (content [this] @shelf)
  (add-book [this book]
            (dosync
             (alter shelf conj book)))
  (remove-book [this book]
               (dosync
                (alter shelf (fn [books] (remove #(= % book) books))))))

; Default library
(def library (RefLibrary. (ref [])))

(deftest add-book-test
  (let [library (RefLibrary. (ref []))
        b {:title "Introduction to Algorithms"
           :authors ["Thomas H. Cormen" "Charles E. Leiserson" "Ronald L. Rivest" "Clifford Stein"]}]
    (add-book library b)
    (is (= (first (content library)) b))))

(deftest remove-book-test
  (let [library (RefLibrary. (ref []))
        b {:title "Artificial Intelligence: A Modern Approach"
           :authors ["Stuart Russell" "Peter Norvig"]}]
    (do
      (add-book library b)
      (remove-book library b)
      (is (empty? library)))))

(run-tests)
