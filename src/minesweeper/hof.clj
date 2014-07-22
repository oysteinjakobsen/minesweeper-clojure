(ns minesweeper.hof
  "Minesweeper Hall of Fame. Uses Neo4j for storage."
  (:require [clojurewerkz.neocons.rest :as rest]
            [clojurewerkz.neocons.rest.cypher :as cypher]
            [clj-time.format :as f]))

(def use-hof
  "Reset this to false if you don't have Neo4j running or don't want the hall-of-fame-functionality."
  (atom true))

(def connection-string 
  "Reset this if your Neo4j instance runs on another server or port."
  (atom "http://localhost:7474/db/data/"))

(def ^{:private true, :const true} cypher-get-hall-of-fame
  "MATCH (g:Game)-[:HAS_LEVEL]->(l:Level {width: {w}, height: {h}, `number-of-mines`: {n}})
   WITH g ORDER BY g.points DESC LIMIT {limit}
   MATCH (p:Player)-[:PLAYED]->(g)
   RETURN ID(g) as id, g.points AS points, p.nick AS nick")

(def ^{:private true, :const true} cypher-add-result
  "CREATE (g:Game {points: {points}, board: {board}})
   MERGE (p:Player {nick: {nick}})
   MERGE (l:Level {width: {w}, height: {h}, `number-of-mines`: {n}})
   MERGE (p)-[:PLAYED]->(g)-[:HAS_LEVEL]->(l)
   WITH g, l
   OPTIONAL MATCH (gg:Game)-[:HAS_LEVEL]->(l)
   WHERE gg.points > g.points
   RETURN ID(g) as id, COUNT(gg)+1 as rank")

(defn- format-board-for-storage
  "Returns a string representation of the board suitable for storage in the database."
  [board]
  (str (assoc board :start-time (f/unparse (f/formatters :date-time-no-ms) (:start-time board)))))

(defn- execute-cypher-table-query
  "Executes the given query and returns the result as a table (list of maps)."
  [cypher bindings]
  (when @use-hof
    (let [conn (rest/connect @connection-string)]
      (cypher/tquery conn cypher bindings))))
  
(defn get-hall-of-fame
  "Returns list of N best results for board of given size and number of mines."
  [width height number-of-mines & [limit]]
  (execute-cypher-table-query 
    cypher-get-hall-of-fame 
    {:w width, :h height, :n number-of-mines, :limit (or limit 10)}))

(defn add-result!
  "Adds a game result. Player and level are added if not already stored. Game id and rank is returned."
  [{:keys [points width height number-of-mines] :as board} nick]
  (first
    (execute-cypher-table-query
      cypher-add-result
      {:nick nick, :board (format-board-for-storage board), :points points, :w width, :h height, :n number-of-mines})))
