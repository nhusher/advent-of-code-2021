(ns part-one
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn to-int [v] (Integer/parseInt v))

;; Convert strings of the form "forward 5" to [:forward 5]
;; to make them easier to manipulate later.
(defn cleanup-input [line]
  (let [[direction distance] (s/split line #" ")]
    [(keyword direction) (to-int distance)]))

;; Given a submarine state and a forward distance to travel
;; return the new depth of the submarine. Acts as a reducer
;; so we can plug it into `reduce` later.
(defmulti  command-executor (fn [s cmd] (first cmd)))
(defmethod command-executor :up      [s [_ delta]] (update s :depth - delta))
(defmethod command-executor :down    [s [_ delta]] (update s :depth + delta))
(defmethod command-executor :forward [s [_ delta]] (update s :distance + delta))

;; Generate a depth/distance result from stdin:
(def result
  (->> (line-seq (io/reader *in*))
       (map cleanup-input)
       (reduce command-executor { :depth 0 :distance 0 })))

;; Get result for the text box:
(* (:depth result) (:distance result))