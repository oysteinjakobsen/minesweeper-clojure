(ns minesweeper.util
  "Collection of utility functions used by the Minesweeper game core."
  (:require [clojure.string :as string]))

(defn number-to-string
  "Converts a number-based index to a string-based index, for example 3 to \"C\"."
  [n]
  (str (char (+ n 64))))

(defn string-to-number
  "Converts a string-based index to a number-based index, for example \"C\" to 3."
  [s]
  (- (int (get (string/upper-case s) 0)) 64))

(defn coordinate-to-index
  "Converts the given coordinate to a vector of indices, for example :B3 to [2 3]."
  [coordinate]
  [(string-to-number (subs (name coordinate) 0 1))
   (read-string (subs (name coordinate) 1))])

(defn index-to-coordinate
  "Converts the given vector of indices to a coordinate, for example [2 3] to :B3."
  [[column row]]
  (keyword (str (number-to-string column) row)))

(defn range-1
  "Returns a one-based range up to and including the given limit. Given 4 as parameter will return (1 2 3 4)."
  [end]
  (range 1 (inc end)))

(defn adjacent-range
  "Given a number N and a board-size S this function will return a range (N-1, N og N+1),
but with a lower limit of 1 and an upper limit of S."
  [n size] 
  (range 
    (max (- n 1) 1)
    (min (+ n 2) (+ size 1))))
