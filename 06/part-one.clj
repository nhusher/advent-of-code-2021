(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string  :as s]))

(defn parse-input [input]
  (into [] (map parse-long (s/split input #","))))

(defn make-fish-state [init]
  (let [empty (into [] (take 9 (repeat 0)))]
    (reduce #(update %1 %2 inc) empty init)))

(defn simulate-day [state]
  (let [[zeroes & rest] state
        next-state (conj (into [] rest) zeroes)] ;; birth new fishies
    (update next-state 6 #(+ % zeroes))))

(defn simulate-days [state days]
  (loop [d days
         s state]
    (if (zero? d) s
      (recur (dec d) (simulate-day s)))))

(defn count-fishes [state] (reduce + state))
(def init (make-fish-state (parse-input (first (line-seq (io/reader *in*))))))

(prn "80d:" (count-fishes (simulate-days init 80)))
(prn "256d:" (count-fishes (simulate-days init 256)))
