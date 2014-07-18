(ns minesweeper.hof
  "Minesweeper Hall of Fame. Uses Neo4j for storage."
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clj-time.format :as f]))

(def ^:dynamic *use-hof*
  "Rebind this to false if you don't have Neo4j running or don't want the hall-of-fame-functionality."
  true)

(def ^:dynamic *connection-string* 
  "Rebind this if your Neo4j runs on another server or port."
  "http://localhost:7474/db/data/")

(defn- tquery
  [conn, cypher, args]
  (get (first (cy/tquery conn cypher args)) "id"))

(defn- format-board-for-storage
  "Returns a string representation of the board suitable for storage in the database."
  [board]
  (str (assoc board :start-time (f/unparse (f/formatters :date-time-no-ms) (:start-time board)))))

(defn get-hall-of-fame
  "Returns list of N best results for board of given size and number of mines."
  [width height number-of-mines & [number]]
  (when *use-hof*
    (let [conn (nr/connect *connection-string*)
          level-id (tquery
                     conn 
                     "MERGE (l:Level {width: {w}, height: {h}, `number-of-mines`: {n}}) RETURN id(l) AS id"
                     {:w width, :h height :n number-of-mines})]
      (cy/tquery 
        conn
        "START l=node({lid}) MATCH (p:Player)-[:PLAYED]->(g:Game)-[:HAS_LEVEL]->(l) RETURN p.nick AS nick, g.points AS points ORDER BY g.points DESC LIMIT {n}" 
        {:lid level-id, :n (or number 5)}))))

(defn add-result!
  "Adds a game result. Player and level are added if not already stored. Game id is returned."
  [board nick]
  (when *use-hof*
    (let [conn (nr/connect *connection-string*)
          level-id (tquery
                     conn 
                     "MERGE (l:Level {width: {w}, height: {h}, `number-of-mines`: {n}}) RETURN id(l) AS id"
                     {:w (:width board), :h (:height board), :n (:number-of-mines board)})
          player-id (tquery
                      conn
                      "MERGE (p:Player {nick: {n}}) RETURN id(p) AS id"
                      {:n nick})]
      (tquery 
        conn 
        "START p=node({pid}), l=node({lid}) CREATE x=(p)-[:PLAYED]->(g:Game {points: {points}, board: {board}})-[:HAS_LEVEL]->(l) RETURN id(g) as id"
        {:pid player-id, :lid level-id, :points (:points board), :board (format-board-for-storage board)}))))
  