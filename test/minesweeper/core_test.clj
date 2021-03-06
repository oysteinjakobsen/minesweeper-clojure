(ns minesweeper.core-test
  "Specification and tests for minesweeper.core"
  (:require [speclj.core :refer :all]
            speclj.run.standard
            [minesweeper.core :refer :all]
            [minesweeper.util :refer :all]
            [clj-time.core :as time]))

(with-redefs
  [time-in-seconds (fn [& _] 42.1)]
  
  (describe
    "number-of-adjacent-mines"
    (with board {:width 5, :height 5, :number-of-mines 2, :number-of-moves 0, 
                 :squares {:A1 :mine, :B1 :sea, :C1 :mine, :D1 :sea, :E1 :sea,
                           :A2 :mine, :B2 :sea, :C2 :mine, :D2 :sea, :E2 :sea,
                           :A3 :sea, :B3 :sea, :C3 :sea, :D3 :sea, :E3 :sea,
                           :A4 :sea, :B4 :mine, :C4 :sea, :D4 :sea, :E4 :sea,
                           :A5 :sea, :B5 :sea, :C5 :sea, :D5 :sea, :E5 :sea}})
    (it
      "returns the number of mines in neighbour squares"
      (should= 0 (number-of-adjacent-mines :D4 @board))
      (should= 4 (number-of-adjacent-mines :B2 @board))
      (should= 1 (number-of-adjacent-mines :D3 @board)))
    (it
      "do not count if there is a mine at the square itself"
      (should= 0 (number-of-adjacent-mines :B4 @board))))
  
  (describe
    "new-board"
    (it 
      "creates a new board with the correct number of mines"
      (should= 13 (count (filter #(= % :mine) (vals (:squares (new-board 7 9 13)))))))
    (it
      "creates squares with sea unless there is a mine"
      (should= (- (* 7 9) 13) (count (filter #(= % :sea) (vals (:squares (new-board 7 9 13)))))))
    (it
      "stores the size and number of mines in the board"
      (let [board (new-board 7 9 13)]
        (should= [7 9 13] [(:width board) (:height board) (:number-of-mines board)])))
    (it
      "creates a board with a minimum width of 5"
      (let [board (new-board 2 20 1)]
        (should= 5 (:width board))))
    (it
      "creates a board with a minimum height of 5"
      (let [board (new-board 20 2 1)]
        (should= 5 (:height board))))
    (it
      "limits the board width to 26"
      (let [board (new-board 7000 9 13)]
        (should= 26 (:width board))))
    (it
      "limits the board height to 50"
      (let [board (new-board 7 900 13)]
        (should= 50 (:height board))))
    (it
      "limits the number of mines to 25% of the total number of squares"
      (let [board (new-board 7 9 1300)]
        (should= (-> (* 7 9) (/ 4) (int)) (:number-of-mines board))))
    (it
      "distributes the mines randomly"
      (let [board (new-board 26 50 300)
            coordinates (board-coordinates 26 50)]
        (should (> 49 (count (partition-by (fn [s] s) (map #(% board) coordinates)))))))
    (it
      "returns a different board each time called"
      (should= 100 (count (distinct (repeatedly 100 #(:squares (new-board 7 9 13)))))))
    (it
      "does not place mines at the given coordinate or on adjacent squares"
      (let [coords-without-mines (conj (adjacent-coordinates :D2 7 9) :D2)]
        (should= [:sea] (distinct (flatten 
                                    (repeatedly 
                                      100 
                                      #(vals (select-keys (:squares (new-board 7 9 100 :D2)) coords-without-mines)))))))))
  
  (describe
    "valid-move?"
    (with board {:width 5, :height 5, :number-of-mines 2, :number-of-moves 0, 
                 :squares {:A1 :mine, :B1 :sea, :C1 :sea, :D1 :sea, :E1 :sea,
                           :A2 :sea, :B2 :sea, :C2 :sea, :D2 :sea, :E2 :sea,
                           :A3 :sea, :B3 :sea, :C3 :sea, :D3 :sea, :E3 :sea,
                           :A4 :sea, :B4 :sea, :C4 :sea, :D4 :mine, :E4 :sea,
                           :A5 :sea, :B5 :sea, :C5 :sea, :D5 :sea, :E5 :sea}})
    (it
      "returns true if this is not the first move"
      (let [board (assoc @board :number-of-moves 5)]
        (should (valid-move? board :A1))))
    (it
      "returns true if this is the first move and there are no mines in adjacent squares or on the square itself"
      (should (valid-move? @board :C2)))
    (it
      "returns false if this is the first move and there's a mine on the square"
      (should-not (valid-move? @board :D4)))
    (it
      "returns false if this is the first move and there are one or more mines on adjacent squares"
      (should-not (valid-move? @board :B2))))
  
  (describe
    "updated-board-state"
    (with board {:width 5, :height 5, :number-of-mines 2, :number-of-moves 3, :seconds 12, :start-time (time/now)}) 
    (with squares {:A1 :flagged-mine, :B1 :sea, :C1 :sea, :D1 :sea, :E1 :sea,
                   :A2 :sea, :B2 :sea, :C2 :sea, :D2 :sea, :E2 :sea,
                   :A3 :sea, :B3 :sea, :C3 :sea, :D3 :sea, :E3 :sea,
                   :A4 :sea, :B4 :sea, :C4 :sea, :D4 :mine, :E4 :sea,
                   :A5 :sea, :B5 :sea, :C5 :sea, :D5 :sea, :E5 :sea})
    (it
      "returns game-state :lost if a mine has exploded"
      (let [board (assoc @board, :squares (assoc @squares, :D4 :exploded))]
        (should= :lost (:game-state (updated-board-state board)))))
    (it
      "returns game-state :won and calculates points if all mines are flagged and nothing else is flagged"
      (let [board (assoc @board :squares (assoc @squares, :A1 :flagged-mine, :D4 :flagged-mine))
            results (updated-board-state board)]
        (should= :won (:game-state results))
        (should= 296 (:points results))))
    (it
      "returns no game-state if game is neither :lost nor :won"
      (let [board (assoc @board, :squares @squares)]
        (should-not (:game-state (updated-board-state board)))))
    (it
      "returns remaining as number of mines minus number of flagged squares"
      (let [board (assoc @board :squares (assoc @squares, :A5 :wrongly-flagged-mine, :D4 :flagged-mine))]
        (should= -1 (:remaining (updated-board-state board)))))
    (it
      "increments number-of-moves"
      (let [board (assoc @board, :squares @squares)]
        (should= 4 (:number-of-moves (updated-board-state board)))))
    (it
      "returns seconds passed since the start of the game"
      (let [board (assoc @board, :squares @squares)]
        (should= 42.1 (:seconds (updated-board-state board))))))
  
  (describe
    "game-over?"
    (with board (new-board 4 4 5))
    (it
      "returns true if the game-state is :won"
      (let [board (assoc @board, :game-state :won)]
        (should (game-over? board))))
    (it
      "returns true if the game-state is :lost"
      (let [board (assoc @board, :game-state :lost)]
        (should (game-over? board))))
    (it
      "otherwise returns false"
      (should-not (game-over? @board))))
  
  (describe
    "merge-boards"
    (with original-board {:width 5, :height 5, :number-of-moves 0, 
                          :squares {:A1 :sea, :B1 :sea,
                                    :A2 :sea, :B2 :mine,
                                    :A3 :sea, :B3 :sea}})
    (with new-partial-board {:number-of-moves 1, :game-state :won,
                             :squares {:A3 :mine, :B3 :flagged-mine}})
    (with merged-board {:width 5, :height 5, :number-of-moves 1, :game-state :won, 
                        :squares {:A1 :sea, :B1 :sea,
                                  :A2 :sea, :B2 :mine,
                                  :A3 :mine, :B3 :flagged-mine}})
    (it
      "merges two boards, taking updated board and square values from the second"
      (should= @merged-board (merge-boards @original-board @new-partial-board))))
  
  (describe
    "do-move"
    (with board {:width 5, :height 5, :number-of-mines 5, :number-of-moves 2, :seconds 12, :start-time (time/now)
                 :squares {:A1 :flagged-mine, :B1 :wrongly-flagged-mine, :C1 :mine, :D1 :sea, :E1 :sea,
                           :A2 :mine, :B2 :sea, :C2 :mine, :D2 :questioned-sea, :E2 :sea,
                           :A3 :sea, :B3 :sea, :C3 :sea, :D3 :sea, :E3 :sea,
                           :A4 :sea, :B4 :mine, :C4 :sea, :D4 :sea, :E4 :sea,
                           :A5 :sea, :B5 :sea, :C5 :sea, :D5 :sea, :E5 :sea}
                 :moves [[:D2 :flag 0.0] [:D2 :flag 1.2] [:B1 :flag 4.7] [:A1 :flag 5.3]]})
    (with explored-coordinates #{:D1 :E1 :E2 :C3 :D3 :E3 :C4 :D4 :E4 :C5 :D5 :E5})
    (it
      "returns coordinates of updated (i.e. exlored) squares"
      (should= @explored-coordinates (set (:updated (do-move @board :E5 :explore)))))
    (it
      "returns state :expored-sea in all explored squares"
      (should= '(:explored-sea) 
               (distinct (vals (select-keys (:squares (do-move @board :E5 :explore)) @explored-coordinates)))))
    (it
      "returns a board with :number-of-moves incremented"
      (should= 3 (:number-of-moves (do-move @board :E5 :explore))))
    (it
      "returns a list of moves with latest move appended"
      (should= [[:D2 :flag 0.0] [:D2 :flag 1.2] [:B1 :flag 4.7] [:A1 :flag 5.3] [:E5 :explore 42.1]]
               (:moves (do-move @board :E5 :explore))))
    (it
      "returns a board with one updated square with state :flagged-mine if move is to flag a mine"
      (let [results (do-move @board :A2 :flag)]
        (should= :flagged-mine (:A2 (:squares results)))
        (should= '(:A2) (:updated results)))) 
    (it
      "returns a board with one updated square with state :questioned-mine if move is to flag an already flagged mine"
      (let [results (do-move @board :A1 :flag)]
        (should= :questioned-mine (:A1 (:squares results)))
        (should= '(:A1) (:updated results)))) 
    (it
      "returns a board with one updated square with state :mine if move is to flag a questioned mine"
      (let [board (assoc @board, :squares (assoc (:squares @board) :A1 :questioned-mine))
            results (do-move board :A1 :flag)]
        (should= :mine (:A1 (:squares results)))
        (should= '(:A1) (:updated results)))) 
    (it
      "returns a board with one updated square with state :questioned-sea if move is to flag an already wrongly flagged mine"
      (let [results (do-move @board :B1 :flag)]
        (should= :questioned-sea (:B1 (:squares results)))
        (should= '(:B1) (:updated results)))) 
    (it
      "returns a board with one updated square with state :sea if move is to flag questioned sea"
      (let [board (assoc @board, :squares (assoc (:squares @board) :B1 :questioned-sea))
            results (do-move board :B1 :flag)]
        (should= :sea (:B1 (:squares results)))
        (should= '(:B1) (:updated results))))
    (it
      "keeps game in progress if last mine was flagged but there are still wrongly flagged mines left"
      (let [board (assoc @board :squares, (assoc (:squares @board), :C1 :flagged-mine, :A2 :flagged-mine, :C2 :flagged-mine, :D2 :sea))
            results (do-move board :B4 :flag)]
        (should= :flagged-mine (:B4 (:squares results)))
        (should-not (:game-state results))))
    (it
      "returns a board with game state :won if last mine was flagged and there are no wrongly flagged mines"
      (let [board (assoc @board, :squares (assoc (:squares @board), :B1 :sea, :C1 :flagged-mine, :A2 :flagged-mine, :C2 :flagged-mine))
            results (do-move board :B4 :flag)]
        (should= :flagged-mine (:B4 (:squares results)))
        (should= #{:B4 :D2} (set (:updated results)))
        (should= :won (:game-state results))))
    (it
      "returns a board with one updated square with state :wrongly-flagged-mine if move is to flag sea as mine"
      (let [results (do-move @board :C4 :flag)]
        (should= '(:C4) (:updated results))
        (should= :wrongly-flagged-mine (:C4 (:squares results)))))
    (it
      "returns a disclosed board with game state :lost if move is to explore a mine"
      (let [results (do-move @board :A2 :explore)]
        (should= :lost (:game-state results))
        (should= :exploded (:A2 (:squares results)))
        (should= #{:B1 :C1 :A2 :C2 :D2 :B4} (set (:updated results)))
        (should= {:B1 :disclosed-wrongly-flagged-mine, :C1 :disclosed-mine
                  :A2 :exploded, :C2 :disclosed-mine, :D2 :sea
                  :B4 :disclosed-mine}
                 (select-keys (:squares results) [:B1 :C1 :A2 :C2 :D2 :B4]))))
    (it
      "creates a new board if first move is to explore a square with a mine or adjacent mines"
      (let [board (assoc @board :number-of-moves 0)]
        (should= :explored-sea (get-in (do-move board :D1 :explore) [:squares :D1]))
        (should= :explored-sea (get-in (do-move board :C2 :explore) [:squares :C2])))))
  
  (describe
    "restructured-board"
    (with board {:width 5, :height 5, :number-of-mines 2, :number-of-moves 2, :updated [:B1 :C2]
                 :squares {:A1 :flagged-mine, :B1 :wrongly-flagged-mine, :C1 :mine, :D1 :sea, :E1 :sea,
                           :A2 :mine, :B2 :explored-sea, :C2 :mine, :D2 :sea, :E2 :sea,
                           :A3 :sea, :B3 :sea, :C3 :sea, :D3 :sea, :E3 :sea,
                           :A4 :sea, :B4 :mine, :C4 :sea, :D4 :sea, :E4 :sea,
                           :A5 :sea, :B5 :explored-sea, :C5 :sea, :D5 :sea, :E5 :sea}})
    (it
      "returns the board's squares as a list of row lists, each square a map containing id (coordinate), state and number of mines (if explored)"
      (should= {:width 5, :height 5, :number-of-mines 2, :number-of-moves 2,
                :squares '(({:id :A1 :state :flagged} {:id :B1 :state :flagged} {:id :C1 :state :untouched} {:id :D1 :state :untouched} {:id :E1 :state :untouched})
                            ({:id :A2 :state :untouched} {:id :B2 :state :explored-sea :mines 4} {:id :C2 :state :untouched} {:id :D2 :state :untouched} {:id :E2 :state :untouched})
                            ({:id :A3 :state :untouched} {:id :B3 :state :untouched} {:id :C3 :state :untouched} {:id :D3 :state :untouched} {:id :E3 :state :untouched})
                            ({:id :A4 :state :untouched} {:id :B4 :state :untouched} {:id :C4 :state :untouched} {:id :D4 :state :untouched} {:id :E4 :state :untouched})
                            ({:id :A5 :state :untouched} {:id :B5 :state :explored-sea :mines 1} {:id :C5 :state :untouched} {:id :D5 :state :untouched} {:id :E5 :state :untouched}))}
               (restructured-board @board)))
    (it
      "returns only updated squares if told so"
      (should= {:width 5, :height 5, :number-of-mines 2, :number-of-moves 2,
                :squares '(({:id :B1 :state :flagged}) ({:id :C2 :state :untouched}))}
               (restructured-board @board {:updates-only? true}))))
  
  (describe
    "mine?"
    (it
      "returns true if a given square state represents a mine"
      (should (mine? :mine))
      (should (mine? :flagged-mine))
      (should (mine? :exploded))
      (should (mine? :disclosed-mine))
      (should (mine? :questioned-mine)))
    (it
      "otherwise returns false"
      (should-not (mine? :sea))
      (should-not (mine? :wrongly-flagged-mine)))))