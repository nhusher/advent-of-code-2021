(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string  :refer [split]]
            [clojure.set     :refer [difference intersection]]))

(defn parse-readout [s]
  (->> (split s #" ")
       (map #(set (map keyword (split % #""))))))

(defn parse-states [s] (sort-by count (parse-readout s)))

(defn parse-line [k]
  (let [[states readout] (split k #" \| ")]
    { :states (parse-states states), :readout (parse-readout readout) }))

(defn intersect [sets] (apply intersection sets))
(defn dif [& ss] (first (apply difference (map #(if (set? %) % (set [%])) ss))))
(defn find-by-count [c coll] (filter #(= (count %) c) coll))
(defn adg [digits] (intersect (find-by-count 5 digits)))
(defn abfg [digits] (intersect (find-by-count 6 digits)))
(defn third [digits] (nth digits 2))

;; Given a set of scrambled digits on the display, marked in seven-segment notation
;; return the corrected seven-segement notation digit
(defmulti find-seg (fn [s _] s))
(defmethod find-seg :a [_ digits] (dif (second digits) (first digits)))
(defmethod find-seg :b [_ digits] (dif (third digits) (find-seg :c digits) (find-seg :d digits) (find-seg :f digits)))
(defmethod find-seg :c [_ digits] (dif (second digits) (abfg digits)))
(defmethod find-seg :d [_ digits] (dif (adg digits) (find-seg :a digits) (find-seg :g digits)))
(defmethod find-seg :e [_ digits] (apply dif (last digits) (map #(find-seg % digits) [:a :b :c :d :f :g])))
(defmethod find-seg :f [_ digits] (dif (first digits) (find-seg :c digits)))
(defmethod find-seg :g [_ digits] (dif (adg digits) (find-seg :a digits) (third digits)))
(defmethod find-seg :default [_ _] nil)

;; Make a map from from scrambled digits to corrected ones
(defn make-map [digits]
  (into {} (map #(vector (find-seg % digits) %) [:a :b :c :d :e :f :g])))

(defn convert-digit [dmap digits] (set (map dmap digits)))

;; Convert a digit representation of segments into an integer
(defn digit->number [digit]
  (case (sort digit)
    [:a :b :c    :e :f :g] 0
    [      :c       :f   ] 1
    [:a    :c :d :e    :g] 2
    [:a    :c :d    :f :g] 3
    [   :b :c :d    :f   ] 4
    [:a :b    :d    :f :g] 5
    [:a :b    :d :e :f :g] 6 
    [:a    :c       :f   ] 7
    [:a :b :c :d :e :f :g] 8
    [:a :b :c :d    :f :g] 9))

(def data (map parse-line (line-seq (io/reader *in*))))

(defn count-simple-numbers [data]
  (let [{ readout :readout, states :states } data
        simple-numbers #{1 4 7 8}
        mapping (make-map states)
        convert #(digit->number (convert-digit mapping %))]
    (count (filter simple-numbers (map convert readout)))))

(prn "part 1: "
  (->> data
    (map count-simple-numbers)
    (reduce +)))

(defn get-readout-number [data]
  (let [{ readout :readout, states :states } data
        mapping (make-map states)
        convert #(digit->number (convert-digit mapping %))]
    (parse-long (apply str (map convert readout)))))

(prn "part 2: "
  (->> data
    (map get-readout-number)
    (reduce +)))