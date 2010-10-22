(ns #^{:doc "The code for exercise 5 of week 1 of the RubyLearning.org 
            Clojure 101 course."
       :author "Daniel Solano Gómez"}
  clojure101.book-list
  (:use clojure.contrib.pprint))

(defstruct #^{:doc "Basic structure for book information."}
  book :title :authors :price)

(def #^{:doc "The top ten Amazon best sellers on 16 Mar 2010."}
  best-sellers
  [(struct book
           "The Big Short"
           ["Michael Lewis"]
           15.09)
   (struct book
           "The Help"
           ["Kathryn Stockett"]
           9.50)
   (struct book
           "Change Your Prain, Change Your Body"
           ["Daniel G. Amen M.D."]
           14.29)
   (struct book
           "Food Rules"
           ["Michael Pollan"]
           5.00)
   (struct book
           "Courage and Consequence"
           ["Karl Rove"]
           16.50)
   (struct book
           "A Patriot's History of the United States"
           ["Larry Schweikart","Michael Allen"]
           12.00)
   (struct book
           "The 48 Laws of Power"
           ["Robert Greene"]
           11.00)
   (struct book
           "The Five Thousand Year Leap"
           ["W. Cleon Skousen","James Michael Pratt","Carlos L Packard","Evan Frederickson"]
           10.97)
   (struct book
           "Chelsea Chelsea Bang Bang"
           ["Chelsea Handler"]
           14.03)
   (struct book
           "The Kind Diet"
           ["Alicia Silverstone","Neal D. Barnard M.D."]
           16.00)])

(defn comma-sep
  "Creates a comma-separated string from a sequence of names."
  [names]
  (apply str (interpose ", " names)))

(def #^{:doc "Formats a double value that is a currency."}
  money-str #(.format (java.text.NumberFormat/getCurrencyInstance) %))

(defn print-book
  "Prints out information about a book."
  [book]
  (println "Title:" (book :title))
  (println "  Author: " (comma-sep (book :authors)))
  (println "  Price:" (money-str (book :price))))

(defn print-book-map
  [{title :title authors :authors, price :price}]
  (println "Title:" title)
  (println "  Author: " (comma-sep authors))
  (println "  Price:" (money-str price)))

(defn print-book-keys
  [{:keys [title authors price]}]
  (println "Title:" title)
  (println "  Author: " (comma-sep authors))
  (println "  Price:" (money-str price)))

(defn print-book-authors
  [{:keys [title authors price]}]
  (println "Title:" title)
  (let [[first second & more] authors]
    (println "  Author:" (comma-sep
                          (filter seq [first
                                       second
                                       (when more "et. al.")]))))
  (println "  Price:" (money-str price)))

(defn print-book-as
  [{:keys [title authors price] :as raw}]
  (println "Title:" title)
  (let [[first second & more] authors]
    (println "  Author:" (comma-sep
                          (filter seq [first
                                       second
                                       (when more "et. al.")]))))
  (println "  Price:" (money-str price))
  (println "  Raw:" (pr-str raw)))

(comment
  (use 'book-list)
  (map print-book best-sellers)
  (map print-book-map best-sellers)
  (map print-book-keys best-sellers)
  (map print-book-authors best-sellers)
  (map print-book-as best-sellers))


(defn print-books [& books]
  (dorun (map print-book (flatten books)))
  (print "\n=============\n")
  (println "Printed information on"
           (count (flatten books))
           "books."))
