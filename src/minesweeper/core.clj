(ns minesweeper.core
  "Minesweeper game core."
  (:require [minesweeper.util :refer :all])
  (:require [clj-time.core :as time]))

(defn number-of-adjacent-mines
  "Returns the number of adjacent mines for a given coordinate."
  [coordinate board]
  (count (filter #(some #{(% board)} '(mine flagged-mine disclosed-mine exploded))
                 (adjacent-coordinates coordinate (:width board) (:height board)))))

(defn coordinates-with-state
  "Returns the list of coordinates for squares on the board having the given state(s)."
  [board state & states]
  (filter
    #(some #{(% board)} (cons state states))
    (keys board)))

(defn change-squares
  "Returns the given board but with updated square states according to the given list of state updates ([from to]*)."
  [board from-to]
  (into board (map #(zipmap 
                      (coordinates-with-state board (first %))
                      (repeat (second %)))
                   from-to)))

(defn game-over?
  "Checks if the game is over and returns 'lost, 'won or nil (meaning game is still in progress)."
  [board]
  (some #{(:board-state board)} '(lost won)))

(defn explore-sea
  "Explores the given sea square and recursively explores adjacent squares. Board updates are returned."
  [board coordinate]
  (loop [new-board {}
         coordinates-to-explore (list coordinate)]
    (let [coordinate (first coordinates-to-explore)
          new-board (conj new-board {coordinate 'explored-sea})
          coordinates-to-explore (into
                                   (set (rest coordinates-to-explore)) 
                                   (if (zero? (number-of-adjacent-mines coordinate board))
                                     (filter #(nil? (% new-board))
                                             (adjacent-coordinates coordinate (:width board) (:height board)))))]
      (if (empty? coordinates-to-explore)
        new-board
        (recur new-board coordinates-to-explore)))))

(defn flag-mine
  "Marks the given square as a mine. Board updates are returned."
  [board coordinate]
  (conj
    {coordinate 'flagged-mine}
    (if (= (count (coordinates-with-state board 'mine)) 1) {:board-state 'won})))

(def boooom (fn [board coordinate] {coordinate 'exploded, :board-state 'lost}))

(def actions {:explore {:mine boooom
                        :flagged-mine boooom
                        :sea explore-sea
                        :wrongly-flagged-mine explore-sea}
              :flag {:mine flag-mine
                     :sea (fn [board coordinate] {coordinate 'wrongly-flagged-mine})}})

(defn do-move
  "Executes the given move on the given square. Returns a complete and updated board."
  [board coordinate action]
  (let [action (or (get-in actions [action (keyword (coordinate board))]) (fn [& ignored] {}))
        new-board (into board (action board coordinate))]
    (if (game-over? new-board)
      (change-squares new-board
                      '([mine disclosed-mine] [wrongly-flagged-mine disclosed-wrongly-flagged-mine]))
      new-board)))

(defn new-board
  "Creates a new board with given size and number of mines. Size is limited to 26 x 50,
 and maximum 25% of the squares will have mines."
  [width height number-of-mines]
  (let [width (min width 26)
        height (min height 50)
        number-of-mines (min number-of-mines (int (/ (* width height) 4)))
        mine-coordinates (list-of-random-coordinates width height number-of-mines)]
    (into
      {:width width, :height height, :number-of-mines number-of-mines :start-time (time/now)}
      (map (fn [coordinate]
             {coordinate (if (some #{coordinate} mine-coordinates) 'mine 'sea)})
           (board-coordinates width height)))))
