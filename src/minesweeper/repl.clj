(ns minesweeper.repl
  "Simple text-based user interface (REPL) for the Minesweeper game."
  (:gen-class)
  (:require [minesweeper.core :refer :all]
            [minesweeper.util :refer :all]
            [clojure.string :as string]))

(defn square-as-string
  "Returns the character that represents the given square on the board."
  [[coordinate state mines]]
  (case state
    untouched " "
    flagged "F"
    explored-sea (if (zero? mines) "." (str mines))
    disclosed-mine "M"
    disclosed-wrongly-flagged-mine "X"
    exploded "*"))

(defn render-board
  "Prints an ascii representation of the given board on the terminal."
  [board]
  (let [width (:width board)
        height (:height board)
        line-as-string (fn [width]
                         (format "   %s+\n" 
                                 (reduce str 
                                         (repeat width "+---"))))
        header-as-string (fn [width]
                           (format "\n%s (secs: %d, moves: %d) %s\n\n   %s\n%s"
                                   "M I N E S W E E P E R"
                                   (:seconds board)
                                   (:number-of-moves board)
                                   (case (game-over? board)
                                     lost "\nSorry, you blew yourself to smithereens :("
                                     won "\nCONGRATS!!!"
                                     nil "")
                                   (reduce str (for [c (range-1 width)] 
                                                 (format "  %s " (number->string c))))
                                   (line-as-string width)))
        row-as-string (fn [index row]
                        (format 
                          "%2s %s|\n"
                          (inc index)
                          (reduce #(str %1 (format "| %s " (square-as-string %2))) "" row)))]
    (println (reduce str
                     (header-as-string width)
                     (map-indexed
                       #(str
                         (row-as-string %1 %2)
                         (line-as-string width)) (:squares board))))))

(defn read-move-from-input
  "Reads a new move from the terminal. The (case insensitive) input is a coordinate (for example 'B3'), 
optionally followed by an action, either \"F\" (flag a mine) or \"E\" (explore, which is default).
The function returns the move on the format [coordinate action] where action is 
either :flag (a mine) or :explore (hopefully just sea)."
  []
  (println "Enter your move (e.g. \"B3\" or \"b3 f\")")
  (let [[coordinate action] (string/split (string/upper-case (read-line)) (re-pattern " "))]
    (when-not (empty? coordinate)
      [(keyword coordinate) (get {"F" :flag, "E" :explore} (or action "E"))])))

(defn play
  "Starts a new game with given board size and number of mines. The board is drawn on and input taken from terminal."
  [width height number-of-mines]
  (loop [board (new-board width height number-of-mines)]
  (render-board (restructure-board board))
  (when-not (game-over? board)
    (when-let [[coordinate action] (read-move-from-input)]
      (recur (merge-boards board (do-move board coordinate action)))))))

(defn -main
  "Runs Minesweeper from the command line. Board width, height, and number of mines must be given as arguments."
  [& args]
  (apply play (map read-string args)))
