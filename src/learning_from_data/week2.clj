(ns learning-from-data.week2
  (:require [clojure.data.generators :as gen]))


(defn gen-binary
  "Generate a binary value - 1 or 0 - with equal probability."
  []
  (if (gen/boolean) 1 0))

(defn gen-binary-seq
  "Generate a lazy sequence of binary values, infinite length or of length n if
   specified."
  ([] (repeatedly gen-binary))
  ([n] (repeatedly n gen-binary)))

(defn mean
  "Takes the mean of a sequence of numerics.  Efficient for long sequences, but
   not numerically stable."
  [bs]
  (let [[sum len] (reduce (fn [[sum len] e]
                            [(+ sum e) (inc len)])
                          [0 0]
                          bs)]
    (/ sum len)))

(defn sign
  "Takes a numeric value and returns the sign, i.e. if the value is positive
   sign returns 1, if the value is negative sign returns -1."
  [n]
  (if (>= n 0) 1 -1))

(defn simulate
  "Simulates flipping n fair coins m times each.  Returns the frequency of
   heads for the coin with the lowest such frequency."
  [n m]
  (let [coins (repeatedly n #(gen-binary-seq m))
        freqs (map mean coins)]
    (apply min freqs)))

(double (mean (repeatedly 100 #(simulate 1000 10))))
