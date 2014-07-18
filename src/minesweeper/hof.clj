(ns minesweeper.hof
  "Minesweeper Hall of Fame. Uses Neo4j for storage."
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clj-time.format :as f]))

(def ^:dynamic *use-hof*
  "Rebind this to false if you don't have Neo4j running or don't want the hall-of-fame-functionality."
  true)

(def ^:dynamic *connection-string* 
  "Rebind this if your Neo4j instance runs on another server or port."
  "http://localhost:7474/db/data/")

(defn- query-returning-id
  "Runs the given Cypher query and returns the id that the query is assumed to return."
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
          level-id (query-returning-id
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
          level-id (query-returning-id
                     conn 
                     "MERGE (l:Level {width: {w}, height: {h}, `number-of-mines`: {n}}) RETURN id(l) AS id"
                     {:w (:width board), :h (:height board), :n (:number-of-mines board)})
          player-id (query-returning-id
                      conn
                      "MERGE (p:Player {nick: {n}}) RETURN id(p) AS id"
                      {:n nick})]
      (query-returning-id 
        conn 
        "START p=node({pid}), l=node({lid}) CREATE path=(p)-[:PLAYED]->(g:Game {points: {pnts}, board: {b}})-[:HAS_LEVEL]->(l) RETURN id(g) AS id"
        {:pid player-id, :lid level-id, :pnts (:points board), :b (format-board-for-storage board)}))))
  