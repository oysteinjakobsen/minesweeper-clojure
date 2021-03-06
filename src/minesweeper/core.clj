(ns minesweeper.core
  "Minesweeper game core."
  (:require [minesweeper.util :refer :all])
  (:require [clj-time.core :as joda]))

(defn mine?
  "Returns true if given state is one that represents a mine, nil otherwise"
  [state]
  (some (partial = state) [:mine :flagged-mine :disclosed-mine :exploded :questioned-mine]))

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
  [{:keys [width height number-of-mines number-of-moves start-time] :as board}]
  (let [number-of-flagges-mines (count (coordinates-with-state board :flagged-mine))
        number-of-wrongly-flagged-mines (count (coordinates-with-state board :wrongly-flagged-mine))
        game-is-won? (and (zero? number-of-wrongly-flagged-mines) (= number-of-flagges-mines number-of-mines))
        game-is-lost? (not (zero? (count (coordinates-with-state board :exploded))))
        game-is-over? (or game-is-won? game-is-lost?)
        seconds (when start-time (time-in-seconds start-time))
        number-of-moves (inc number-of-moves)]
    (conj board
          [:number-of-moves number-of-moves]
          [:remaining (- number-of-mines (+ number-of-flagges-mines number-of-wrongly-flagged-mines))]
          [:seconds (or seconds 0.0)]
          (when (= number-of-moves 1)
            [:start-time (joda/now)])
          (when game-is-over?
            [:game-state (or 
                           (when game-is-won? :won) 
                           (when game-is-lost? :lost))])
          (when game-is-won?
            [:points (-> 
                       (* width height)
                       (* number-of-mines)
                       (/ seconds)
                       (/ number-of-moves)
                       (* 1000)
                       (int))]))))

(defn game-over?
  "Checks if the game is over (returns :lost or :won) or still in progress (returns nil)."
  [{:keys [game-state]}]
  (some #{game-state} [:lost :won]))

(defn merge-boards
  "Merges changes from new-partial-board into the original-board."
  [original-board new-partial-board]
  (assoc (merge original-board new-partial-board)
         :squares (merge (:squares original-board) (:squares new-partial-board))))

(defn explore-sea
  "Explores the given sea square and recursively explores adjacent squares. Board updates are returned."
  [{:keys [width height squares] :as board} coordinate]
  (loop [board-updates {:squares {}}
         coords-to-explore (list coordinate)]
    (let [coordinate (first coords-to-explore)
          board-updates (merge-boards board-updates {:squares {coordinate :explored-sea}})
          square-is-sea-and-not-already-visited? (fn [coord] 
                                                   (and
                                                     (= (coord squares) :sea)
                                                     (nil? (coord (:squares board-updates)))))
          coords-to-explore (into
                              (set (rest coords-to-explore))
                              (when (zero? (number-of-adjacent-mines coordinate board))
                                (filter square-is-sea-and-not-already-visited?
                                        (adjacent-coordinates coordinate width height))))]
      (if (empty? coords-to-explore)
        board-updates
        (recur board-updates coords-to-explore)))))

(defn valid-move?
  "A first move to a square with mine or adjacent mines is not valid. All other moves are valid."
  [{:keys [number-of-moves squares] :as board} coordinate]
  (or (not (zero? number-of-moves))
      (and (not= (coordinate squares) :mine)
           (zero? (number-of-adjacent-mines coordinate board)))))

(defn disclosed-board
  "Returns board updates with real states of flagged and questioned squares."
  [board]
  (change-squares board
                  [[:mine :disclosed-mine]
                   [:wrongly-flagged-mine :disclosed-wrongly-flagged-mine]
                   [:questioned-sea :sea]
                   [:questioned-mine :disclosed-mine]]))

(defn- generate-squares
  "Generates a map of squares with mines randomly distributed. If a coordinates is passed then
 that and all adjacent squares will be free of mines."
  [width height number-of-mines coordinate]
  (let [coords-without-mines (when coordinate 
                               (conj (adjacent-coordinates coordinate width height) coordinate))
        coords (group-by
                 #(if (some #{%} coords-without-mines) :without :with)
                 (board-coordinates width height))]
    (zipmap
      (into (shuffle (:with coords)) (:without coords))
      (concat (repeat number-of-mines :mine) (repeat :sea)))))

(defn new-board
  "Creates a new board with given size and number of mines. Size is limited to be
 between 5 x 5 and 26 x 50, and maximum 25% of the squares will have mines.
 If a coordinate is passed then that and all adjacent squares will be free of mines."
  [width height number-of-mines & [coordinate]]
  (let [width (max 5 (min width 26))
        height (max 5 (min height 50))
        number-of-mines (min number-of-mines (int (/ (* width height) 4)))]
    {:width width, :height height, :number-of-mines number-of-mines, :number-of-moves 0, :remaining number-of-mines
     :squares (generate-squares width height number-of-mines coordinate)}))

(def ^{:private true, :const true} transitions
  "Maps from action (explore or flag) and a square state to a new square state or a function returning multiple new square states."
  {:explore {:mine :exploded
             :sea explore-sea}
   :flag {:mine :flagged-mine
          :sea :wrongly-flagged-mine
          :flagged-mine :questioned-mine
          :wrongly-flagged-mine :questioned-sea
          :questioned-sea :sea
          :questioned-mine :mine}})

(defn do-move
  "Executes the given move on the given square. Returns a complete and updated board."
  [{:keys [width height number-of-mines squares moves] :as board} coordinate action]
  (if-not (valid-move? board coordinate)
    (let [board (new-board width height number-of-mines coordinate)]
      (merge-boards board (do-move board coordinate action)))
    (let [operation (get-in transitions [action (coordinate squares)] (fn [& _] {}))
          operation-results (if (fn? operation) (operation board coordinate) {:squares {coordinate operation}})
          updated-squares (keys (:squares operation-results))
          board-updates (updated-board-state (merge-boards board operation-results))
          board-updates (assoc board-updates
                               :updated updated-squares
                               :moves (conj (or moves []) [coordinate action (:seconds board-updates)]))]
      (if (game-over? board-updates)
        (merge-boards board-updates (disclosed-board board-updates))
        board-updates))))

(defn- restructured-square
  "Returns the given square as a map containing id (coordinate), anonymized state, and number of mines (if explored)."
  [{:keys [squares] :as board} coordinate]
  (let [state (coordinate squares)
        ; Anonymized state means a state that does not reveal the presence of a mine 
        anonymized-state (case (coordinate squares)
                           (:mine :sea) :untouched
                           (:flagged-mine :wrongly-flagged-mine) :flagged
                           (:questioned-mine :questioned-sea) :questioned
                           state)]
    (conj {:id coordinate, :state anonymized-state}
          (when (= anonymized-state :explored-sea)
            [:mines (number-of-adjacent-mines coordinate board)]))))

(defn restructured-board
  "Restructures the board for viewing, organizing it row-by-row and including necessary (but anonymized) information."
  [{:keys [width height updated] :as board} & [{:keys [updates-only?] :or {updates-only? false}}]]
  (let [board-coordinates (board-coordinates width height)
        coordinates (if updates-only? 
                      (filter #(some #{%} updated) board-coordinates) 
                      board-coordinates)]
    (dissoc (assoc board
                   :squares (partition-by #(coordinate->row-index (:id %))
                                          (map (partial restructured-square board) coordinates)))
            :start-time :moves :updated)))