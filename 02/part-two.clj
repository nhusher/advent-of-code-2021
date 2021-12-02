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
;; return the new depth of the submarine
(defn new-depth [s delta] (* (:aim s) delta))

;; Create a multimethod command executor that acts as a reducer
;; taking in a submarine state and updating it with a new state
(defmulti command-executor (fn [s cmd] (first cmd)))
(defmethod command-executor :up      [s [_ delta]] (update s :aim - delta))
(defmethod command-executor :down    [s [_ delta]] (update s :aim + delta))
(defmethod command-executor :forward [s [_ delta]]
  ;; New thing: depth is modified by the distance and current :aim value
  (-> s
      (update :distance + delta)
      (update :depth + (new-depth s delta))))

;; Run commands against the result:
(def result
  (->> (line-seq (io/reader *in*))
       (map cleanup-input)
       (reduce command-executor { :aim 0 :depth 0 :distance 0 })))

;; Get result for the text box:
(* (:depth result) (:distance result))