(defproject oysteinj/minesweeper "1.0.0-SNAPSHOT"
  :description "Minesweeper game"
  :url "https://github.com/oysteinjakobsen/minesweeper-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.7.0"]
                 [org.clojars.pjlegato/clansi "1.3.0"]
                 [clojurewerkz/neocons "3.1.0-SNAPSHOT"]
                 [org.clojure/tools.cli "0.3.1"]]
  :profiles {:dev {:dependencies [[speclj "3.0.1"]]}}
  :plugins [[speclj "3.0.1"]]
  :test-paths ["test"]
  :main minesweeper.repl
  :aot [minesweeper.repl])
