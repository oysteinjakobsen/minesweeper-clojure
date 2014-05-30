(ns minesweeper.util
  "Samling med utility-funksjoner som brukes av spillkjernen."
  (:require [clojure.string :as string]))

(defn tall-til-streng
  "Konverterer en tallbasert indeks til en bokstavbasert indeks, f.eks. 3 til \"C\"."
  [t]
  (str (char (+ t 64))))

(defn streng-til-tall
  "Konvererter en bokstavbasert indeks til en tallbasert indeks, f.eks. \"C\" til 3."
  [s]
  (- (int (get (string/upper-case s) 0)) 64))

(defn koordinat-til-indeks
  "Gitt en koordinat så returneres indekser som [kolonne bredde]. F.eks. vil :B3 returneres som [2 3]."
  [koordinat]
  [(streng-til-tall (subs (name koordinat) 0 1))
   (read-string (subs (name koordinat) 1))])

(defn indeks-til-koordinat
  "Gitt indekser som [kolonne bredde] så returneres koordinat. F.eks. vil [2 3] returneres som :B3."
  [[kolonne rad]]
  (keyword (str (tall-til-streng kolonne) rad)))

(defn range-1
  "Returnerer en range f.o.m. 1 t.o.m. gitt størrelse. F.eks. vil størrelse 4 returnere (1 2 3 4)."
  [end]
  (range 1 (inc end)))

(defn nabo-range
  "Gitt et tall N og en brettstørrelse S så returneres en range N-1, N og N+1, men begrenset nedad til 1 og oppad til S."
  [n storrelse] 
  (range 
    (max (- n 1) 1)
    (min (+ n 2) (+ storrelse 1))))
