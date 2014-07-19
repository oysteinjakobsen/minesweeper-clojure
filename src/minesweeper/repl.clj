(ns minesweeper.repl
  "Simple text-based user interface (REPL) for the Minesweeper game. If you want to see clean and
beautiful Clojure code then look elsewhere: Dive into core and util instead :)"
  (:gen-class)
  (:require [minesweeper.core :refer :all]
            [minesweeper.util :refer :all]
            [minesweeper.hof :refer :all]
            [clojure.string :as string]
            [clansi.core :as ansi]))

(def ^{:private true, :const true} colors
  "Maps from number of mines to a color for displaying that number."
  {1 :blue, 2 :green, 3 :red, 4 :magenta})

(defn square-as-string
  "Returns the character that represents the given square on the board."
  [[coordinate state mines]]
  (case state
    untouched " "
    flagged "F"
    questioned "?"
    explored-sea (if (zero? mines) "." (ansi/style (str mines) (get colors mines :default)))
    disclosed-mine (ansi/style "M" :red :bright)
    disclosed-wrongly-flagged-mine (ansi/style "X" :red :bright)
    exploded (ansi/style "*" :red :bright)))

(defn render-board
  "Prints an ascii representation of the given board on the terminal."
  [{:keys [width height seconds number-of-moves remaining points squares] :as board}]
  (let [line-as-string (fn [width]
                         (format "   %s+\n" 
                                 (reduce str 
                                         (repeat width "+---"))))
        header-as-string (fn [width]
                           (format "\n%s (secs: %d, moves: %d, remaining: %d) %s\n\n   %s\n%s"
                                   "M I N E S W E E P E R"
                                   (or seconds 0)
                                   number-of-moves
                                   remaining
                                   (case (game-over? board)
                                     lost (ansi/style "\nSorry, you blew yourself to smithereens :(" :red :bright)
                                     won (ansi/style (str "\nCONGRATS!!! - " points " points") :green :bright)
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
                          (line-as-string width)) squares)))))

(defn render-hall-of-fame
  "Renders the hall of fame, i.e. list of best results for the given board size and number of mines."
  [{:keys [width height number-of-mines]}]
  (when *use-hof*
    (println (str "\n* HALL OF FAME *\n"
                  (reduce str (map 
                                (fn [entry]
                                  (format "%4d %s\n" (get entry "points") (get entry "nick")))
                                (get-hall-of-fame width height number-of-mines)))))))

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

(defn read-nick-from-input
  "Read player's nick from the terminal."
  []
  (when *use-hof*
    (println "Enter your nick")
    (string/trim (string/lower-case (read-line)))))
  
(defn play
  "Starts a new game with given board size and number of mines. The board is drawn on and input taken from terminal."
  [width height number-of-mines & options]
  (binding [ansi/use-ansi (if (some #{'-c} options) true false)
            *use-hof* (if (some #{'-hof} options) true false)]
    (loop [board (new-board width height number-of-mines)]
      (render-board (restructure-board board))
      (if-not (game-over? board)
        (when-let [[coordinate action] (read-move-from-input)]
          (recur (do-move board coordinate action)))
        (when (= (game-over? board) 'won)
          (when-let [nick (read-nick-from-input)]
            (add-result! board nick))
          (render-hall-of-fame board))))))

(defn -main
  "Runs Minesweeper from the command line. Board width, height, and number of mines must be given as arguments."
  [& args]
  (apply play (map read-string args)))
