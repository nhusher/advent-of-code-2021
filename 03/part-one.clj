(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;; Given a seq of 0 or 1 members, convert to an integer:
(defn binary-to-int [bits] (Integer/parseInt (s/join "" bits) 2))

;; Convert lines of bit strings to lists of 0 or 1
(defn clean-input [line] (map parse-long (s/split line #"")))

;; Reducer function that gets the 0 or 1 bias of a set of binary digits
(defn count-bit [acc it] (if (zero? it) (dec acc) (inc acc)))

;; A reducer function that takes a list of bit counts and returns
;; a new list where each bit in the bit string modifies the count
;; up or down by one.
(defn count-bits [bit-counts bit-string]
  (map #(count-bit %1 %2) bit-counts bit-string))

(defn rate-fn [pred? bit-counts]
  (map #(if (pred? %) 1 0) bit-counts))

(defn gamma-rate [bit-counts]
  (rate-fn pos? bit-counts))

(defn epsilon-rate [bit-counts]
  (rate-fn neg? bit-counts))

(def bit-counts
     (->> (line-seq (io/reader *in*))
          (map clean-input)
          (reduce count-bits (take 12 (repeat 0)))))

;; 4160394
(*
  (binary-to-int (gamma-rate bit-counts))
  (binary-to-int (epsilon-rate bit-counts)))

bit-counts