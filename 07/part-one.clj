
(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string  :as s]))

(defn parse-input [input]
  (map parse-long (s/split input #",")))

(defn abs [i] (if (neg? i) (* i -1) i))

(defn rotate [v] (apply map vector v))

(defn linear-fuel-costs [size position]
  (map #(abs (- % position)) (range (inc size))))

(defn all-linear-fuel-costs [data]
  (let [size (apply max data)]
    (map #(apply + %) (rotate (map #(linear-fuel-costs size %) data)))))

(def data (parse-input (first (line-seq (io/reader *in*)))))

(prn "part 1:" (apply min (all-linear-fuel-costs data)))

(defn triangular [n] (/ (* n (dec n)) 2))

(defn triangular-fuel-costs [size position]
  (map #(triangular (inc (abs (- % position)))) (range (inc size))))

(defn all-triangular-fuel-costs [data]
  (let [size (apply max data)]
    (map #(apply + %) (rotate (map #(triangular-fuel-costs size %) data)))))

(prn "part 2:" (apply min (all-triangular-fuel-costs data)))