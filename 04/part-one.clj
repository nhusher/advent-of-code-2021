(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string  :as s]
            [clojure.set     :refer [superset? difference]]))

;;
;; Parses the input format into usable data:
;;
(defn parse-numbers [command-str]
  (map parse-long (s/split command-str #",")))

(defn parse-board [board]
  (map #(map parse-long (s/split (s/trim %) #"\s+")) board))

(defn parse-boards [boards]
  (->> boards
    (partition-by count)
    (remove #(= (count %) 1))
    (map parse-board)))

(defn parse-input [[first & rest]]
  (let [numbers (parse-numbers first)
        boards   (parse-boards rest)]
    [numbers boards]))

(defn rotate [l] (apply map vector l))

;; Given a board and a list of called bingo numbers,
;; return true if this board will have won using those numbers
(defn winner? [numbers board]
  (let [board-r    (rotate board)
        number-set (set numbers)]
    (or
      (some #(superset? number-set (set %)) board)
      (some #(superset? number-set (set %)) board-r))))

;; Return a seq of the unmarked cells in a bingo card, given
;; a list of numbers that have been called. Used for generating
;; a final score.
(defn unmarked [numbers board]
  (let [number-set (set numbers)]
    (mapcat #(difference (set %) number-set) board)))

;; Generate a seq of which boards have won, which have not yet won
;; and what numbers have been called. Winners and and non-winners are
;; stored as sets.
(defn winner-seq [numbers boards]
  (map
    (fn [n]
      (let [nums (take n numbers)]
        { :numbers nums, 
          :winning-boards (set (filter #(winner? nums %) boards))
          :remaining-boards (set (remove #(winner? nums %) boards))}))
    (range 1 (inc (count numbers)))))

;; Yeild the first winner from the winner seq:
(defn first-winner [winner-seq]
   (->> winner-seq
        (filter #(not= #{} (:winning-boards %)))
        first))

;; Generate a score given a single board 
(defn generate-score [winner]
  (let [{ boards :winning-boards, nums :numbers } winner]
    (* (last nums) (apply + (unmarked nums (first boards))))))

;; Read lines, convert to seq:
(let [[numbers boards] (parse-input (line-seq (io/reader *in*)))]
  (def winners (winner-seq numbers boards)))

(prn " part 1:"
  (-> winners
      first-winner
      generate-score))

(defn last-winner [winner-seq]
  (let [final-pair (->> winner-seq
                        (partition 2 1)
                        (filter #(= (count (:remaining-boards (second %))) 0))
                        first)
        final-nums (:numbers (second final-pair))
        final-board (:remaining-boards (first final-pair))]
    { :numbers final-nums, :winning-boards final-board }))

(prn "part 2:"
  (-> winners
      last-winner
      generate-score))
