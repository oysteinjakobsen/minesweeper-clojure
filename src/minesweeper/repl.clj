(ns minesweeper.repl
  "Simple text-based user interface (REPL) for the Minesweeper game."
  (:gen-class)
  (:require [minesweeper.core :refer :all]
            [minesweeper.util :refer :all]
            [clojure.string :as string]
            [clj-time.core :as time]))

(defn square-as-string
  "Returns the character that represents the given square on the board."
  [board coordinate]
  (case (coordinate board)
    (sea mine) " "
    (flagged-mine wrongly-flagged-mine) "F"
    explored-sea (let [number (number-of-adjacent-mines coordinate board)]
                   (if (zero? number) "." (str number)))
    disclosed-mine "M"
    disclosed-wrongly-flagged-mine "X"
    exploded "*"))

(defn board-as-string
  "Returns a string representation of the given board."
  [board]
  (let [width (:width board)
        height (:height board)
        line-as-string (fn [width]
                         (format "   %s+\n" 
                                 (reduce str 
                                         (repeat width "+---"))))
        header-as-string (fn [width]
                           (format "\n%s (%d secs) %s\n\n   %s\n%s"
                                   "M I N E S W E E P E R"
                                   (time/in-seconds (time/interval (:start-time board) (time/now)))
                                   (case (game-over? board)
                                     lost "Sorry, you blew yourself to smithereens :("
                                     won "CONGRATS!!!"
                                     nil "")
                                   (reduce str (for [c (range-1 width)] 
                                                 (format "  %s " (number-to-string c))))
                                   (line-as-string width)))
        row-as-string (fn [board row]
                        (format 
                          "%2s %s|\n"
                          row
                          (reduce 
                            str 
                            (for [column (range-1 width)] 
                              (format "| %s " (square-as-string board (index-to-coordinate [column row])))))))]
    (reduce str
            (header-as-string width)
            (for [row (range-1 height)]
              (str
                (row-as-string board row)
                (line-as-string width))))))

(defn read-move-from-input
  "Reads a new move from the terminal. The (case insensitive) input is a coordinate (for example 'B3'), 
optionally followed by an action, either \"F\" (flag a mine) or \"E\" (explore, which is default).
The function returns the move on the format [coordinate action] where action is 
either :flag (a mine) or :explore (hopefully just sea)."
  []
  (let [[coordinate action] (string/split (string/upper-case (read-line)) (re-pattern " "))]
    [(if (empty? coordinate) nil (keyword coordinate))
     (get {"F" :flag, "E" :explore} (or action "E"))]))

(defn play
  "Starts a new game with given board size and number of mines. The board is drawn on and input taken from terminat."
  [width height number-of-mines]
  (loop [board (new-board width height number-of-mines)]
    (println (board-as-string board))
    (if-not (game-over? board)
      (do
        (println "Enter your move (e.g. \"B3\" or \"b3 f\")")
        (let [[coordinate action] (read-move-from-input)]
          (if (not (nil? coordinate))
            (recur (do-move board coordinate action))))))))

(defn -main
  "Runs Minesweeper from the command line. Board width, height, and number of mines must be given as arguments."
  [& args]
  (apply play (map read-string args)))
