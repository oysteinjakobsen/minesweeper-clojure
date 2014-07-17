(ns minesweeper.core
  "Minesweeper game core."
  (:require [minesweeper.util :refer :all])
  (:require [clj-time.core :as time]))

(defn mine?
  "Returns true if given state is one that represents a mine, nil otherwise"
  [state]
  (some #(= state %) '(mine flagged-mine disclosed-mine exploded questioned-mine)))

(defn number-of-adjacent-mines
  "Returns the number of adjacent mines on the given board for a given coordinate."
  [coordinate {:keys [squares width height]}]
  (count (filter #(mine? (% squares)) (adjacent-coordinates coordinate width height))))

(defn- coordinates-with-state
  "Returns the list of coordinates for squares on the given board having the given state(s)."
  [{:keys [squares]} & states]
  (filter #(some #{(% squares)} states) (keys squares)))

(defn change-squares
  "Returns a partial board with updated states according to the given list of state updates ([from to]*)."
  [board from-to]
  (let [squares (reduce merge {} (map #(zipmap
                                         (coordinates-with-state board (first %))
                                         (repeat (second %)))
                                      from-to))]
    (assoc {:squares squares} :updated (into (or (:updated board) {}) (keys squares)))))

(defn updated-board-state
  "Updates the board with state, i.e. seconds passed, number of moves, remaining mines, game-state, and points."
  [board]
  (let [number-of-flagges-mines (count (coordinates-with-state board 'flagged-mine))
        number-of-wrongly-flagged-mines (count (coordinates-with-state board 'wrongly-flagged-mine))
        game-is-won? (and (zero? number-of-wrongly-flagged-mines) (= number-of-flagges-mines (:number-of-mines board)))
        game-is-lost? (not (zero? (count (coordinates-with-state board 'exploded))))
        game-is-over? (or game-is-won? game-is-lost?)
        seconds (when-let [start-time (:start-time board)] (time-in-seconds start-time))
        number-of-moves (inc (:number-of-moves board))]
    (conj board
          [:number-of-moves number-of-moves]
          [:remaining (- (:number-of-mines board) (+ number-of-flagges-mines number-of-wrongly-flagged-mines))]
          [:seconds (or seconds 0)]
          (when (= number-of-moves 1)
            [:start-time (time/now)])
          (when game-is-over?
            [:game-state (or 
                           (when game-is-won? 'won) 
                           (when game-is-lost? 'lost))])
          (when game-is-won?
            [:points (-> 
                       (* (:width board) (:height board))
                       (* (:number-of-mines board))
                       (/ seconds)
                       (/ number-of-moves)
                       (* 1000)
                       (int))]))))

(defn filter-board
  "Returns a filtered board with only updated squares."
  [board]
  (dissoc (assoc board :squares (select-keys (:squares board) (:updated board)) :updated)))

(defn game-over?
  "Checks if the game is over (returns 'lost or 'won) or still in progress (returns nil)."
  [{:keys [game-state]}]
  (some #{game-state} '(lost won)))

(defn merge-boards
  "Merges changes from new-partial-board into the original-board."
  [original-board new-partial-board]
  (assoc (merge original-board new-partial-board)
         :squares (merge (:squares original-board) (:squares new-partial-board))))

(defn- explore-sea
  "Explores the given sea square and recursively explores adjacent squares. Board updates are returned."
  [board coordinate]
  (loop [new-board {:squares {}}
         coordinates-to-explore (list coordinate)]
    (let [coordinate (first coordinates-to-explore)
          new-board (merge-boards new-board {:squares {coordinate 'explored-sea}})
          coordinates-to-explore (into
                                   (set (rest coordinates-to-explore))
                                   (when (zero? (number-of-adjacent-mines coordinate board))
                                     (filter #(nil? (% (:squares new-board)))
                                             (adjacent-coordinates coordinate (:width board) (:height board)))))]
      (if (empty? coordinates-to-explore)
        new-board
        (recur new-board coordinates-to-explore)))))

(def actions {:explore {:mine 'exploded
                        :flagged-mine 'exploded
                        :sea explore-sea
                        :wrongly-flagged-mine explore-sea}
              :flag {:mine 'flagged-mine
                     :sea 'wrongly-flagged-mine
                     :flagged-mine 'questioned-mine
                     :wrongly-flagged-mine 'questioned-sea
                     :questioned-sea 'sea
                     :questioned-mine 'mine}})

(defn valid-move?
  "A first move to a square with mine or adjacent mines is not valid. All other moves are valid."
  [board coordinate]
  (or (not (zero? (:number-of-moves board)))
      (and (zero? (number-of-adjacent-mines coordinate board))
           (not= (coordinate (:squares board)) 'mine))))

(defn disclosed-board
  [board]
  (change-squares board
                  '([mine disclosed-mine]
                     [wrongly-flagged-mine disclosed-wrongly-flagged-mine]
                     [questioned-sea sea]
                     [questioned-mine disclosed-mine])))

(declare new-board)

(def no-op (fn [& _] {}))

(defn do-move
  "Executes the given move on the given square. Returns a complete and updated board."
  [board coordinate action]
  (if-not (valid-move? board coordinate)
    (let [board (new-board (:width board) (:height board) (:number-of-mines board))]
      (merge-boards board (do-move board coordinate action)))
    (let [action (or (get-in actions [action (keyword (coordinate (:squares board)))]) no-op)
          action-results (if (fn? action) (action board coordinate) {:squares {coordinate action}})
          updated-squares (keys (:squares action-results))
          new-board (assoc (updated-board-state (merge-boards board action-results)) :updated updated-squares)]
      (if (game-over? new-board)
        (merge-boards new-board (disclosed-board new-board))
        new-board))))

(defn new-board
  "Creates a new board with given size and number of mines. Size is limited to 26 x 50,
 and maximum 25% of the squares will have mines."
  [width height number-of-mines]
  (let [width (min width 26)
        height (min height 50)
        number-of-mines (min number-of-mines (int (/ (* width height) 4)))]
    {:width width, :height height, :number-of-mines number-of-mines, :number-of-moves 0, :remaining number-of-mines
     :squares (zipmap
                (shuffle (board-coordinates width height))
                (concat (repeat number-of-mines 'mine) (repeat 'sea)))}))

(defn- anonymize-square
  "Anonymizes the state so that the client don't see the mines."
  [square]
  (let [square (replace {'mine 'untouched, 'sea 'untouched, 'flagged-mine 'flagged, 'wrongly-flagged-mine 'flagged 'questioned-mine 'questioned 'questioned-sea 'questioned} square)]
    (if (= (second square) 'untouched)
      (assoc square 2 0)
      square)))

(defn- restructure-square
  "Returns the given square as a vector containing coordinate, state, and number-of-mines."
  [board coordinate]
  [coordinate (coordinate (:squares board)) (number-of-adjacent-mines coordinate board)])

(defn restructure-board
  "Restructures the board for viewing, organizing it row-by-row and including necessary information."
  [board]
  (dissoc (assoc board 
                 :squares (partition-by #(second (coordinate->index (first %)))
                                        (map #(anonymize-square (restructure-square board %))
                                             (board-coordinates (:width board) (:height board))))
                 :seconds (time-in-seconds (:start-time board)))
          :start-time))