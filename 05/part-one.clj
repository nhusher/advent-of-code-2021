(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string  :as s]))

(defn parse-line [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    [{ :x (parse-long x1) :y (parse-long y1)},
     { :x (parse-long x2) :y (parse-long y2)}]))

(def example-data (map parse-line example))

(defn dimension [dim data]
  (mapcat #(vector (-> %1 first :x) (-> %1 second dim)) data))

(defn straight? [path]
  (let [[{ x1 :x, y1 :y } { x2 :x, y2 :y }] path]
    (or (= x1 x2) (= y1 y2))))

(defn grid-range [start end]
  (let [end'   (if (< start end) (inc end) (dec end))
        step   (if (> start end) -1 1)]
      (range start end' step)))

(defn empty-map [w h]
  { :w w, :h h, :d (into [] (repeat (* w h) 0)) })

(defn point-to-index [m point]
  (let [{x :x, y :y} point
        { w :w }     m]
    (+ (* w y) x)))

(defn apply-changes [m points]
  (reduce #(update-in %1 [:d (point-to-index m %2)] inc) m points))

(defn straight-path [path]
  (let [[{ x1 :x, y1 :y } { x2 :x, y2 :y }] path]
    (if (= x1 x2)
      (map (fn [x y] { :x x, :y y }) (repeat x1) (grid-range y1 y2))
      (map (fn [x y] { :x x, :y y }) (grid-range x1 x2) (repeat y1)))))

(defn diagonal-path [path]
  (let [[{ x1 :x, y1 :y } { x2 :x, y2 :y }] path]
    (map (fn [x y] { :x x, :y y }) (grid-range x1 x2) (grid-range y1 y2))))

(defn apply-straight-path [m path]
  (if-not
    (straight? path) m
    (apply-changes m (straight-path path))))

(defn apply-path [m path]
  (apply-changes m
    (if (straight? path)
      (straight-path path)
      (diagonal-path path))))

(defn generate-map [pather data]
  (let [min-x 0
        min-y 0
        max-x (inc (apply max (dimension :x data)))
        max-y (inc (apply max (dimension :y data)))
        m     (empty-map max-x max-y)]
    (reduce pather m data)))

(defn format-row [row]
  (map #(if (zero? %) "." %) row))

(defn print-map [m]
  (let [{ w :w, h :h, d :d } m
        rows (map format-row (partition w d))]
    (doseq [row rows]
      (prn (s/join "" row)))))

(def lines (map parse-line (line-seq (io/reader *in*))))


(prn "part 1"
  (->> lines
       (generate-map apply-straight-path)
       :d
       (filter #(< 1 %))
       count))


(prn "part 2"
  (->> lines
       (generate-map apply-path)
       :d
       (filter #(< 1 %))
       count))
