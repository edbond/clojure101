(ns clojure101.mp3
  (:use [clojure.test])
  (:use [clojure.contrib.pprint :only [cl-format]])
  (:refer-clojure)
  (:import [java.io File RandomAccessFile]))

(def working-directory
     (-> (File. *file*) (.getParent)))

(defn full-filename
  [filename]
  (-> (File. working-directory) (File. filename)))

(defn bytes-to-string
  [array]
  (->>
   (areduce array i chars []
            (let [c (aget array i)]
              (if-not (= c 0)
                (conj chars (char c))
                chars)))
   (apply str)))

(defn read-string
  [file offset length]
  (let [array (byte-array length)]
    (.seek file offset)
    (.read file array 0 length)
    (bytes-to-string array)))

(defn has-id3-tag?
  [filename]
  (let [file (RandomAccessFile. (full-filename filename) "r")
        length (.length file)
        offset (- length 128)]
    (= (read-string file offset 3) "TAG")))

(defn tags
  [filename]
  {:pre (has-id3-tag? filename)}
  (let [file (RandomAccessFile. (full-filename filename) "r")
        length (.length file)
        id3-start (- length 128)
        name (+ id3-start 3)
        artist (+ name 30)
        album (+ artist 30)
        year (+ album 30)
        r (partial read-string file)]
    {"Song" (r name 30)
     "Artist" (r artist 30)
     "Album" (r album 30)
     "Year" (r year 4)}))

(defn pp-tags
  [filename]
  (cl-format nil "ID3v1 Tags for ~A~%~A~%~:{~10<~A~>: ~A~%~}~A~%"
             filename
             (apply str (repeat 50 \=))
             (seq (tags filename))
             (apply str (repeat 50 \-))))

(deftest has-id3-test
  (is (has-id3-tag? "song.mp3")))

(deftest pp-tags-test
  (is (= "ID3v1 Tags for song.mp3
==================================================
      Song: Dancing Shoes
    Artist: Cliff Richard and The Shadows
     Album: (SUMMER HOLIDAY  1963)
      Year: 2000
--------------------------------------------------
" (pp-tags "song.mp3"))))
