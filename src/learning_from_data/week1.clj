(ns learning-from-data.week1)

(defn rand-point
  "Generate a random point in the given dimensions."
  [dimensions]
  (letfn [(random [[l u]] (+ l (rand (- u l))))]
    (vec
      (map random dimensions))))

(defn dot-product
  "Takes the dotproduct of input vectors."
  [& vectors]
  (apply + (apply map * vectors)))

(defn slope
  [[x1 y1] [x2 y2]]
  (/ (- y2 y1)
     (- x2 x1)))

(defn intercept
  [m [x y]]
  (- y (* m x)))

(defn rand-f
  "Create a random linear target function over dimensions."
  [dimensions]
  (let [[x1 y1] (rand-point dimensions)
        [x2 y2] (rand-point dimensions)
        m (/ (- y2 y1)
             (- x2 x1))
        b (- y1 (* m x1))]
    (fn [[x y]]
      (if (> y (+ (* m x) b))
        1
        -1))))

(defn scorer [w x]
  (if (neg? (dot-product w (into [1] x)))
    -1 1))
(defn miss? [w [x y]] (not= y (scorer w x)))

(defn perceptron
  "Takes a training set and returns a function which takes a set of weights
   and updates them according to the perceptron learing algorithm."
  [dataset]
  (fn [w]
    (if-let [[x y] (first (filter (partial miss? w) dataset))]
      (vec
        (map + w
             (map (partial * y) (into [1] x))))
      w)))

(def X [[-1 1] [-1 1]])
(def f (rand-f X))
(def inputs (repeatedly #(rand-point X)))
(def dataset (map vector inputs (map f inputs)))

(take 10 inputs)
(take 10 dataset)

(def g (perceptron (take 10 dataset)))
(def training (iterate g [0 0 0]))
(take 10 training)

(defn count-steps
  [weights]
  (loop [count 0
         last nil
         remaining weights]
    (if (= last (first remaining))
      count
      (recur (inc count) (first remaining) (rest remaining)))))

(defn count-steps-itr []
  (let [X [[-1 1] [-1 1]]
        f (rand-f X)
        inputs (repeatedly #(rand-point X))
        training-set (map vector (take 100 inputs) (map f inputs))
        g (perceptron training-set)]
    (count-steps (iterate g [0 0 0]))))

(defn count-steps-misses []
  (let [X [[-1 1] [-1 1]]
        f (rand-f X)
        inputs (repeatedly #(rand-point X))
        training-set (map vector (take 100 inputs) (map f inputs))
        verification-set (map vector (take 1000 (drop 100 inputs)) (map f inputs))
        g (perceptron training-set)
        weights (iterate g [0 0 0])
        n (count-steps weights)
        w (nth weights n)]
    (/ (count (filter (partial miss? w) verification-set))
       (count verification-set))))


(/
  (reduce +
          (take 1000 (repeatedly count-steps-itr)))
  1000.0)


(comment
  y = 0.3513604552685565 x + 0.19607510816996448

  )

(def f (rand-f [[-1 1] [-1 1]]))
(f [-0.5 -0.5])
(f (rand-point [[-1 1] [-1 1]]))
