(ns minesweeper.kjerne
  "Minesweeper spillkjerne."
  (:require [minesweeper.util :refer :all])
  (:require [clj-time.core :as time]))

(defn finn-nabo-koordinater
  "Gitt en koordinat så returneres en liste med alle nabo-koordinatene - 3, 5 eller 8 i antall."
  [koordinat brett]
  (let [[kolonne rad] (koordinat-til-indeks koordinat)]
    (filter
      #(not= koordinat %)
      (for [k (nabo-range kolonne (:bredde brett)) 
            r (nabo-range rad (:hoyde brett))]
        (indeks-til-koordinat [k r])))))

(defn antall-nabo-miner
  "Gitt en koordinat så returneres antall miner på nabo-koordinatene."
  [koordinat brett]
  (count (filter #(some #{%} '(mine markert-mine avslort-mine))
                 (map #(% brett)
                      (finn-nabo-koordinater koordinat brett)))))

(defn tilfeldig-koordinat
  "Returnerer en tilfeldig koordinat på det gitte brettet."
  [brett]
  (let [tilfeldig-tall (comp inc int rand)]
    (indeks-til-koordinat
      [(tilfeldig-tall (:bredde brett))
       (tilfeldig-tall (:hoyde brett))])))

(defn legg-miner
  "Legger ut riktig antall miner på brettet og returnerer minenes koordinater."
  [brett]
  (loop [koordinater #{}]
    (if (= (count koordinater) (:antall-miner brett))
      koordinater
      (recur (conj koordinater (tilfeldig-koordinat brett))))))

(defn koordinater-med-status
  "Returnerer en liste med de koordinater på brettet som har gitt(e) status(er)."
  [brett status & statuser]
  (filter 
    #(some #{(% brett)} (cons status statuser))
    (keys brett)))

(defn legg-til-manglende-felter
  "Returnerer et brett som inneholder nytt-brett supplert med manglende felter fra gammelt-brett."
  [nytt-brett gammelt-brett]
  (into
    nytt-brett
    (filter
      #(nil? ((first %) nytt-brett))
      gammelt-brett)))

(defn endre-felt-status
  "Returnerer gitt brett med nye felt-statuser i.h.t. til gitte fra-til-status-par."
  [brett & fra-til]
  (loop [nytt-brett {}
         fra-til fra-til]
    (let [fra (first fra-til)
          til (second fra-til)
          fra-til (drop 2 fra-til)
          nytt-brett (into nytt-brett (zipmap (koordinater-med-status brett fra) (repeat til)))]
      (if (empty? fra-til)
        (legg-til-manglende-felter nytt-brett brett)
        (recur nytt-brett fra-til)))))

(defn boooom
  "Spillet er tapt. Status 'tapt returneres."
  [brett koordinat]
  {:status 'tapt})

(defn klarer-nabofelter
  "Klarer gitt felt som minefritt og gjør rekursivt det samme med nabofeltene. Brettoppdateringer returneres."
  [brett koordinat]
  (loop [nytt-brett {}
         koordinater (list koordinat)]
    (let [koordinat (first koordinater)
          nytt-brett (conj nytt-brett {koordinat 'markert-sjo})
          koordinater (set (into (rest koordinater) 
                                 (if (zero? (antall-nabo-miner koordinat brett))
                                   (filter #(nil? (% nytt-brett))
                                           (finn-nabo-koordinater koordinat brett)))))]
      (if (empty? koordinater)
        nytt-brett
        (recur nytt-brett koordinater)))))

(defn flagg-mine
  "Markerer at det er en mine på angitt koordinat, og returnerer brettoppdateringer."
  [brett koordinat]
  (conj
    {koordinat 'markert-mine}
    (if (= (count (koordinater-med-status brett 'mine)) 1) {:status 'vunnet} nil)))

(defn feilmarker-mine
  "Markerer at det er en feilmarkert mine på angitte koordinater, og returnerer brettoppdateringer."
  [brett koordinat]
  {koordinat 'feilmarkert-mine})

(defn gjor-ingenting
  "Gjør ingenting og returnerer gitte brettoppdateringer uendret."
  [brett koordinat]
  {})

(defn spillet-er-slutt
  "Sjekker om spillet er slutt og returnerer enten 'tapt, 'vunnet eller nil (ikke slutt)."
  [brett]
  (some #{(:status brett)} '(tapt vunnet)))

(def handlinger {:klarer {:mine boooom, :sjo klarer-nabofelter, :feilmarkert-mine klarer-nabofelter}
                 :flagg {:mine flagg-mine, :sjo feilmarker-mine}})

(defn utfor-handling
  "Utfører et trekk (gitt handling på gitt koordinat) og returnerer nytt og komplett spillbrett med status."
  [brett koordinat handling]
  (let [aksjon (or (get-in handlinger [handling (keyword (koordinat brett))]) gjor-ingenting)
        nytt-brett (legg-til-manglende-felter (aksjon brett koordinat) brett)]
    (if (spillet-er-slutt nytt-brett)
      (endre-felt-status nytt-brett 'mine 'avslort-mine 'feilmarkert-mine 'avslort-feilmarkert-mine)
      nytt-brett)))

(defn nytt-brett
  "Lager og returnerer et nytt brett med angitt bredde, høyde og antall miner.
Brettet begrenses i størrelse til 26 x 50 felter, og maksimalt halvparten av feltene minelegges."
  [bredde hoyde antall-miner]
  (let [bredde (min bredde 26)
        hoyde (min hoyde 50)
        antall-miner (min antall-miner (int (/ (* bredde hoyde) 2)))
        tomt-brett {:bredde bredde, :hoyde hoyde, :antall-miner antall-miner :start-tid (time/now)}
        mine-koordinater (legg-miner tomt-brett)
        felt-verdi (fn [koordinat mine-koordinater] (if (some #{koordinat} mine-koordinater) 'mine 'sjo))]
    (into
      tomt-brett
      (for [k (range-1 bredde)
            r (range-1 hoyde)]
        (let [koordinat (indeks-til-koordinat [k r])
              verdi (felt-verdi koordinat mine-koordinater)]
          {koordinat verdi})))))
