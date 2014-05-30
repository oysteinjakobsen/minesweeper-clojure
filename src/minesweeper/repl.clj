(ns minesweeper.repl
  "Tekstbasert grensesnitt (repl) for å spille Minesweeper."
  (:require [minesweeper.kjerne :refer :all]
            [minesweeper.util :refer :all]
            [clojure.string :as string]
            [clj-time.core :as time]))

(defn tegn-felt
  "Beregner og returnerer tegnet som skal brukes for uttegning av gitt koordinat på brettet."
  [brett koordinat]
  (case (koordinat brett)
    (sjo mine) " "
    (markert-mine feilmarkert-mine) "F"
    markert-sjo (let [antall-nabo-miner (antall-nabo-miner koordinat brett)]
                  (if (zero? antall-nabo-miner) "." (str antall-nabo-miner)))
    avslort-mine "M"
    avslort-feilmarkert-mine "X"))

(defn tegn-brett
  "Tegner opp angitt brett som tekst."
  [brett]
  (let [bredde (:bredde brett)
        hoyde (:hoyde brett)
        tegn-linje (fn [bredde]
                     (format "   %s+\n" 
                             (reduce str 
                                     (repeat bredde "+---"))))
        tegn-topp (fn [bredde]
                    (format "%s (%d sekunder) %s\n\n   %s\n%s"
                            "M I N E S W E E P E R"
                            (time/in-seconds (time/interval (:start-tid brett) (time/now)))
                            (case (spillet-er-slutt brett)
                              tapt "Du tapte dessverre :("
                              vunnet "GRATULERER!!!"
                              nil "")
                            (reduce str (for [k (range-1 bredde)] 
                                          (format "  %s " (tall-til-streng k))))
                            (tegn-linje bredde)))
        tegn-rad (fn [brett rad]
                   (format "%2s %s|\n"
                           rad
                           (reduce str 
                                   (for [k (range-1 bredde)] 
                                     (format "| %s " (tegn-felt brett (indeks-til-koordinat [k rad])))))))]
    (reduce str
            (tegn-topp bredde)
            (for [r (range-1 hoyde)]
              (str
                (tegn-rad brett r)
                (tegn-linje bredde))))))

(defn les-trekk
  "Leser et trekk på formen [koordinat handling*]."
  []
  (let [[koordinat handling] (string/split (string/upper-case (read-line)) (re-pattern " "))]
    [(if (empty? koordinat) nil (keyword koordinat))
     (get {"F" :flagg, "K" :klarer} (or handling "K"))]))

(defn spill
  "REPL for å spille. Brettets størrelse og antall miner angis."
  [hoyde bredde antall-miner]
  (loop [brett (nytt-brett hoyde bredde antall-miner)]
    (println (tegn-brett brett))
    (if (not (spillet-er-slutt brett))
      (let [[koordinat handling] (les-trekk)]
        (if (not (nil? koordinat))
          (recur (utfor-handling brett koordinat handling)))))))
