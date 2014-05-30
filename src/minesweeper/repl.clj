(ns minesweeper.repl
  "Text-based user interface (REPL) for Minesweeper."
  (:require [minesweeper.core :refer :all]
            [minesweeper.util :refer :all]
            [clojure.string :as string]
            [clj-time.core :as time]))

(defn draw-square
  "Returns the character that represents the given square on the board."
  [board coordinate]
  (case (coordinate board)
    (sea mine) " "
    (flagged-mine wrongly-flagged-mine) "F"
    explored-sea (let [number-of-adjacent-mines (number-of-adjacent-mines coordinate board)]
                   (if (zero? number-of-adjacent-mines) "." (str number-of-adjacent-mines)))
    disclosed-mine "M"
    wrongly-disclosed-mine "X"
    exploded "*"))

(defn draw-board
  "Returns a string representation of the given board."
  [board]
  (let [width (:width board)
        height (:height board)
        draw-line (fn [width]
                    (format "   %s+\n" 
                            (reduce str 
                                    (repeat width "+---"))))
        draw-header (fn [width]
                      (format "%s (%d secs) %s\n\n   %s\n%s"
                             "M I N E S W E E P E R"
                             (time/in-seconds (time/interval (:start-time board) (time/now)))
                             (case (game-is-over board)
                               lost "Sorry, you blew yourself to smithereens :("
                               won "CONGRATS!!!"
                               nil "")
                              (reduce str (for [c (range-1 width)] 
                                            (format "  %s " (number-to-string c))))
                              (draw-line width)))
        draw-row (fn [board row]
                   (format "%2s %s|\n"
                           row
                           (reduce str 
                                   (for [column (range-1 width)] 
                                     (format "| %s " (draw-square board (index-to-coordinate [column row])))))))]
    (reduce str
            (draw-header width)
            (for [row (range-1 height)]
              (str
                (draw-row board row)
                (draw-line width))))))

(defn read-move-input
  "Reads a new move from the terminal and returns it on the format [coordinate action*]."
  []
  (let [[coordinate action] (string/split (string/upper-case (read-line)) (re-pattern " "))]
    [(if (empty? coordinate) nil (keyword coordinate))
     (get {"F" :flag, "E" :explore} (or action "E"))]))

(defn play
  "Starts a new game with given board size and number of mines."
  [height width number-of-mines]
  (loop [board (new-board height width number-of-mines)]
    (println (draw-board board))
    (if (not (game-is-over board))
      (let [[coordinate action] (read-move-input)]
        (if (not (nil? coordinate))
          (recur (do-move board coordinate action)))))))
