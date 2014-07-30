(ns minesweeper.analysis
  "Module for analyzing already played games."
  (:require [minesweeper.core :refer :all]))

(defn- reset-board
  "Resets a finished (won) board to it's initial state."
  [board]
  (dissoc 
    (assoc
      (merge-boards
        board
        (change-squares 
          board
          [[:explored-sea :sea] [:flagged-mine :mine] [:questioned-mine :mine] [:questioned-sea :sea]]))
      :remaining (:number-of-mines board)
      :number-of-moves 0
      :replay true)
    :start-time :points :game-state :seconds :moves :updated))

(defn- extract-moves-from-board
  "Returns the moves recorded in the given finished board.
   If time for each move is missing it is calculated as an average."
  [{:keys [moves seconds number-of-moves]}]
  (if (= 2 (count (first moves)))
    (let [average-time (/ seconds (dec number-of-moves))]
      (map-indexed #(conj %2 (* %1 average-time)) moves))
    moves))

(defn prepare-board-for-replay
  "Given a finished (won) board, it turns the initial board and moves separately in a map."
  [board]
  {:board (reset-board board)
   :moves (extract-moves-from-board board)})
