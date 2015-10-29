(ns solitaire.core
  (:require [merpg.2D.core :as c]
            [merpg.2D.make-game :as g]

            [clojure.pprint :refer :all]))

(defn luo-pakka []
  (shuffle (for [maa [:hertta :ruutu :pata :risti]
                  arvo (range 13)]
              {:maa maa
               :arvo arvo})))

(def pakka-ref
  "0. indeksi on pakan päällimmäinen"
  (ref (luo-pakka)))

(def visible-pakka-ref (ref []))


;;vasemmalta oikealle indeksilukusuoralla = ylhäältä alas ruudulla
(def hidden-pino-refs (repeatedly 7 (partial ref [])))
(def visible-pino-refs (repeatedly 7 (partial ref [])))

(defn get-hidden-pino [n]
  (try
    (or (nth hidden-pino-refs n)
        (println  n ". pino rikki"))
    (catch IndexOutOfBoundsException ex
      (println "IOOBE - n is " n)
      (throw ex))))

(defn get-visible-pino [n]
  (try
    (or (nth visible-pino-refs n)
        (println  n ". näkyvä pino rikki"))
    (catch IndexOutOfBoundsException ex
      (println "IOOBE - n is " n)
      (throw ex))))

(defn jaa! []
  (dosync
   (alter pakka-ref (constantly (luo-pakka)))
   (alter visible-pakka-ref (constantly []))
   (dotimes [x 7]
     (alter (get-hidden-pino x) (constantly []))
     (alter (get-visible-pino x) (constantly []))))
   
  
  (dosync
   (dotimes [n 7]
     (let [kortit (take (inc n) @pakka-ref)]
       ;; Tiputetaan kortit pakasta
       (alter pakka-ref (partial drop (inc n)))
       ;;siirretään ne pinoihin
       (alter (get-hidden-pino n)
              (partial concat kortit))))))

(defn indeksit-joissa-kääntöä-tarvitaan []
  (->> (range 7)
       (filter #(empty? @(get-visible-pino %)))))

(defn käännä! [indeksi]
  (dosync
   (let [kortti (first @(get-hidden-pino indeksi))]
     (alter (get-hidden-pino indeksi) (partial drop 1))
     (alter (get-visible-pino indeksi) (partial cons kortti)))))

(defn hoida-käännöt! []
  (dosync
   (doseq [i (indeksit-joissa-kääntöä-tarvitaan)]
     (käännä! i))))

(defn siirrä!
  "Siirrä päällimmäinen pinosta x pinoon y"
  [x y]
  (dosync
   (when-not (empty? @(get-visible-pino x))
     (let [kortti (first @(get-visible-pino x))]
       (alter (get-visible-pino x) (drop 1))
       (alter (get-visible-pino y) (partial cons kortti))))))
