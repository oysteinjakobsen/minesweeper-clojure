(defproject minesweeper "1.0.0-SNAPSHOT"
  :description "Minesweeper game"
  :url "https://github.com/oysteinjakobsen/minesweeper-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-time "0.7.0"]]
  :main minesweeper.repl
  :aot [minesweeper.repl])
