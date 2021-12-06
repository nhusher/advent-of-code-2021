(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string  :as s]
            [clojure.set     :refer [superset?]]))


(def lines (line-seq (io/reader *in*)))

;;
;; Parses the input format into usable data:
;;
(defn parse-numbers [command-str]
  (map parse-long (s/split command-str #",")))

(defn parse-board [board]
  (map #(into #{} (map parse-long (s/split (s/trim %) #"\s+"))) board))

(defn parse-boards [boards]
  (->> boards
    (partition-by count)
    (remove #(= (count %) 1))
    (map parse-board)))

(defn parse-input [[first & rest]]
  (let [numbers (parse-numbers first)
        boards   (parse-boards rest)]
    [numbers boards]))


(def board (first (second (parse-input lines))))

(defn rotate [l] (apply map (comp set vector) l))

;; Given a board and a list of called bingo numbers,
;; return true if this board will have won using those numbers
(defn winner? [numbers board]
  (let [board-r    (rotate board)
        number-set (set numbers)]
    (or
      (some #(superset? number-set %) board)
      (some #(superset? number-set %) board-r))))

(parse-input lines)