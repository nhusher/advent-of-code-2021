(ns part-two
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;; Given a seq of 0 or 1 members, convert to an integer:
(defn binary-to-int [bits] (Integer/parseInt (s/join "" bits) 2))

;; Convert lines of bit strings to lists of 0 or 1
(defn clean-input [line] (map parse-long (s/split line #"")))

;; Reducer function that gets the 0 or 1 bias of a set of binary digits
(defn count-bit [acc it] (if (zero? it) (dec acc) (inc acc)))

(defn nth-bit-commonality [n lines]
  (let [bit-count (reduce count-bit 0 (map #(nth % n) lines))]
    (if (neg? bit-count) 0 1)))

(defn life-support-fn [pred lines]
  (loop [n     0
         c     (nth-bit-commonality n lines)
         lines (filter #(pred (nth % n) c) lines)]
    (let [[first & rest] lines]
      (if (empty? rest)
        first
        (let [n' (inc n)
              c' (nth-bit-commonality n' lines)]
          (recur n' c' (filter #(pred (nth % n') c') lines)))))))

(def lines
     (->> (line-seq (io/reader *in*))
          (map clean-input)))

(defn oxygen-generator [lines]
  (life-support-fn = lines))

(defn co2-scrubber [lines]
  (life-support-fn not= lines))

;; 4125600
(*
  (binary-to-int (oxygen-generator lines))
  (binary-to-int (co2-scrubber lines)))