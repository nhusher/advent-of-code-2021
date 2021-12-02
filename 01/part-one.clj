(ns part-one
  (:require [clojure.java.io :as io]))

(defn to-int [v] (Integer/parseInt v))

(->> (map to-int (line-seq (io/reader *in*)))
     (partition 2 1)       ;; split into marching doubles
     (filter #(apply < %)) ;; only keep where A < B
     count)                ;; count the result
