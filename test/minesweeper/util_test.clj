(ns minesweeper.util-test
  "Specification and tests for minesweeper.util"
  (:require [speclj.core :refer :all]
            [minesweeper.util :refer :all]))

(describe
  "number->string"
  (it
    "converts a base-1 index to an alphabetical index"
    (should= "A" (number->string 1))
    (should= "Z" (number->string 26))))

(describe
  "coordinate->index"
  (it
    "converts a coordinate to a base-1 index in a case-insensitive way"
    (should= [4 7] (coordinate->index :D7))
    (should= [4 7] (coordinate->index :d7))))

(describe
  "index->coordinate"
  (it
    "converts a base-1 index to coordinate"
    (should= :D7 (index->coordinate [4 7]))
    (should= :A3 (index->coordinate [1 3]))
    (should= :Z1 (index->coordinate [26 1]))))

(describe
  "coordinate->row-index"
  (it
    "should return the row index of the given coordinate"
    (should= 17 (coordinate->row-index :D17))))

(describe
  "range-1"
  (it
    "returns a range with base 1"
    (should= '(1 2 3 4) (range-1 4))))

(describe
  "adjacent-coordinates"
  (it
    "returns all eight neighbour coordinates"
    (should= '(:B2 :C2 :D2 :B3 :D3 :B4 :C4 :D4) (adjacent-coordinates :C3 5 5)))
  (it
    "only returns three coordinates given a corner coordinate"
    (should= '(:B1 :A2 :B2) (adjacent-coordinates :A1 5 5))
    (should= '(:D4 :E4 :D5) (adjacent-coordinates :E5 5 5)))
  (it
    "only returns five coordinates given a border coordinate"
    (should= '(:B1 :D1 :B2 :C2 :D2) (adjacent-coordinates :C1 5 5))
    (should= '(:B4 :C4 :D4 :B5 :D5) (adjacent-coordinates :C5 5 5))
    (should= '(:A2 :B2 :B3 :A4 :B4) (adjacent-coordinates :A3 5 5))
    (should= '(:D2 :E2 :D3 :D4 :E4) (adjacent-coordinates :E3 5 5))))

(describe
  "board-coordinates"
  (it
    "returns a list of all board coordinates, row by row"
    (should= '(:A1 :B1 :A2 :B2 :A3 :B3) (board-coordinates 2 3))))
