(ns clojure101.processes_log
  (:use clojure.java.io
        [clojure.contrib.io :as io :only [read-lines]]
        [clojure.contrib.str-utils :only [re-split]]))

(def cwd
     (-> (file *file*) (.getParent)))
(def *filename* (str cwd "/" "processes.log"))

(def current-chunk (ref nil))
(def log-records (atom []))

(defn chunk-id
  "Returns chunk id if line starts a new chunk"
  [line]
  (if-let [[chunk-data] (re-seq #"### (.+) (.+) (.+) ###" line)]
    (rest chunk-data)))

(defn chunk-key
  "Set chunk key and return key for partition"
  [line]
  (if-let [chunk-data (chunk-id line)]
    (dosync
     (ref-set current-chunk chunk-data))
    @current-chunk))

(defn discard-line
  [line]
  (re-matches #"^$|^\.+$" line))

(defn store-chunk
  "Store log chunk"
  [whole-chunk]
  (let [chunk (remove discard-line whole-chunk)
        id (chunk-id (first chunk))
        records (drop 2 chunk)
        log-vec (map #(re-split #"\s+" %) records)
        store-fn (fn [log]
                   (swap! log-records conj (concat id log)))]
    (map #(store-fn %) log-vec)))

(defn parse-log
  [filename]
  (->>
   (io/read-lines (file filename))
   (partition-by chunk-key)
   (map store-chunk)))

(parse-log *filename*)
