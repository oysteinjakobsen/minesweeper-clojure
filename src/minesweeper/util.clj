(ns minesweeper.util
  "Collection of utility functions used by the Minesweeper game core."
  (:require [clojure.string :as string]
            [clj-time.core :as joda]))

(defn number->string
  "Converts a number-based index to a string-based index, for example 3 to \"C\"."
  [n]
  (-> n (+ 64) (char) (str)))

(defn- string->number
  "Converts a string-based index to a number-based index, for example \"C\" to 3."
  [s]
  (-> s (string/upper-case) (get 0) (int) (- 64)))

(defn coordinate->index
  "Converts the given coordinate to a vector of indices, for example :B3 to [2 3]."
  [coordinate]
  [(string->number (subs (name coordinate) 0 1))
   (read-string (subs (name coordinate) 1))])

(defn coordinate->row-index
  "Convenience function that returns the row index of the given coordinate. For example :B3 yields 3."
  [coordinate]
  (second (coordinate->index coordinate)))

(defn index->coordinate
  "Converts the given vector of indices to a coordinate, for example [2 3] to :B3."
  [[column row]]
  (keyword (str (number->string column) row)))

(defn range-1
  "Returns a one-based range up to and including the given limit. Given 4 as parameter will return (1 2 3 4)."
  [end]
  (range 1 (inc end)))

(defn- adjacent-range
  "Given a number N and a board-size S this function will return a range (N-1, N og N+1),
but with a lower limit of 1 and an upper limit of S."
  [n size]
  (range (max (dec n) 1)
         (inc (min (inc n) size))))

(defn adjacent-coordinates
  "Given a coordinate this function returns a list of adjacent coordinates; 3, 5 or 8 in number."
  [coordinate width height]
  (let [[column row] (coordinate->index coordinate)]
    (filter (partial not= coordinate)
            (for [r (adjacent-range row height)
                  c (adjacent-range column width)]
              (index->coordinate [c r])))))

(defn board-coordinates
  "Returns a list of coordinates representing each square on a board of given size."
  [width height]
  (for [row (range-1 height)
        col (range-1 width)]
    (index->coordinate [col row])))

(defn time-in-seconds
  "Returns a decimal number of seconds passed since the given start time."
  [start-time]
  (float (/ (joda/in-millis (joda/interval start-time (joda/now))) 1000)))
