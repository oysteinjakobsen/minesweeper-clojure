(ns minesweeper.kjerne
  "Minesweeper game core."
  (:require [minesweeper.util :refer :all])
  (:require [clj-time.core :as time]))

(defn find-adjacent-coordinates
  "Given a coordinate this function returns a list of adjacent coordinates; 3, 5 or 8 in number."
  [coordinate board]
  (let [[column row] (coordinate-to-index coordinate)]
    (filter
      #(not= coordinate %)
      (for [c (adjacent-range column (:width board)) 
            r (adjacent-range row (:height board))]
        (index-to-coordinate [c r])))))

(defn number-of-adjacent-mines
  "Returns the number of adjacent mines for a given coordinate."
  [coordinate board]
  (count (filter #(some #{%} '(mine flagged-mine disclosed-mine))
                 (map #(% board)
                      (find-adjacent-coordinates coordinate board)))))

(defn random-coordinate
  "Returns a random coordinate on the given board."
  [board]
  (let [random-number (comp inc int rand)]
    (index-to-coordinate
      [(random-number (:width board))
       (random-number (:height board))])))

(defn place-mines
  "Returns a list of random coordinates for mines on the board."
  [board]
  (loop [coordinates #{}]
    (if (= (count coordinates) (:number-of-mines board))
      coordinates
      (recur (conj coordinates (random-coordinate board))))))

(defn coordinates-with-state
  "Returns the list of coordinates for squares on the board having the given state(s)."
  [board state & states]
  (filter 
    #(some #{(% board)} (cons state states))
    (keys board)))

(defn add-missing-board-data
  "Returns a board based on new-board, and with missing data supplied from old-board."
  [new-board old-board]
  (into
    new-board
    (filter
      #(nil? ((first %) new-board))
      old-board)))

(defn change-squares
  "Returns the given board but with updated square states according to the given from-to pairs."
  [board & from-to]
  (loop [new-board {}
         from-to from-to]
    (let [from (first from-to)
          to (second from-to)
          from-to (drop 2 from-to)
          new-board (into new-board (zipmap (coordinates-with-state board from) (repeat to)))]
      (if (empty? from-to)
        (add-missing-board-data new-board board)
        (recur new-board from-to)))))

(defn boooom
  "Game over..."
  [board coordinate]
  {:board-state 'lost})

(defn explore-square
  "Explores the given sea square and recursively explores adjacent squares. Board updates are returned."
  [board coordinate]
  (loop [new-board {}
         coordinates (list coordinate)]
    (let [coordinate (first coordinates)
          new-board (conj new-board {coordinate 'explored-sea})
          coordinates (set (into (rest coordinates) 
                                 (if (zero? (number-of-adjacent-mines coordinate board))
                                   (filter #(nil? (% new-board))
                                           (find-adjacent-coordinates coordinate board)))))]
      (if (empty? coordinates)
        new-board
        (recur new-board coordinates)))))

(defn flag-mine
  "Marks the given square as a mine. Board updates are returned."
  [board coordinate]
  (conj
    {coordinate 'flagged-mine}
    (if (= (count (coordinates-with-state board 'mine)) 1) {:board-state 'won} nil)))

(defn wrongly-flag-mine
  "Wrongly marks the given square as a mine. Board updates are returned."
  [board coordinate]
  {coordinate 'wrongly-flagged-mine})

(defn no-op
  "Do nothing..."
  [board coordinate]
  {})

(defn game-is-over
  "Checks if the game is over and returns 'lost, 'won or nil (meaning game is still in progress)."
  [board]
  (some #{(:board-state board)} '(lost won)))

(def actions {:explore {:mine boooom, :sea explore-square, :wrongly-flagged-mine explore-square}
              :flag {:mine flag-mine, :sea wrongly-flag-mine}})

(defn do-move
  "Executes the given move on the given square. Returns a complete and updated board."
  [board coordinate action]
  (let [action (or (get-in actions [action (keyword (coordinate board))]) no-op)
        new-board (add-missing-board-data (action board coordinate) board)]
    (if (game-is-over new-board)
      (change-squares new-board 'mine 'disclosed-mine 'wrongly-flagged-mine 'disclosed-wrongly-flagged-mine)
      new-board)))

(defn new-board
  "Creates a new board with given size and number of mines. Size is limited to 26 x 50,
 and maximum 50% of the squares will have mines."
  [width height number-of-mines]
  (let [width (min width 26)
        height (min height 50)
        number-of-mines (min number-of-mines (int (/ (* width height) 2)))
        empty-board {:width width, :height height, :number-of-mines number-of-mines :start-time (time/now)}
        mine-coordinates (place-mines empty-board)
        square-state (fn [coordinate mine-coordinates] (if (some #{coordinate} mine-coordinates) 'mine 'sea))]
    (into
      empty-board
      (for [c (range-1 width)
            r (range-1 height)]
        (let [coordinate (index-to-coordinate [c r])
              state (square-state coordinate mine-coordinates)]
          {coordinate state})))))
