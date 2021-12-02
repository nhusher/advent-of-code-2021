(ns part-two
  (:require [clojure.java.io :as io]))

(defn to-int [v] (Integer/parseInt v))

(->> (line-seq (io/reader *in*)) ;; get a lazy seq of lines
     (map to-int)                ;; convert every line to an integer
     (partition 3 1)             ;; split into marching triples
     (map #(apply + %))          ;; sum each triple
     (partition 2 1)             ;; create marching doubles
     (filter #(apply < %))       ;; filter out where A >= B
     count)                      ;; count the result
