(ns eul.core
  (:gen-class))

(defn -main
  []
  (println "Hello, World!"))



(defn prima?
  [x]
  (let [akar (Math/sqrt x)
        iter (fn iter [i]
               (cond (> i akar) true
                     (zero? (rem x i)) false
                     :else (iter (+ i 2))))]
    (cond (<= x 1) false
          (= x 2) true
          (even? x) false
          :else (iter 3))))
(defn eul7
  [o]
  (->> (range 1000000)
       (filter prima?)
       (take o)
       (last)))

(defn eul10
  [q]
  (->> (range q)
       (filter prima?)
       (reduce +)))

(def fibonacci (lazy-cat [0 1] (map +' fibonacci (rest fibonacci)))) 

(defn kali
  [x y]
  (apply *' (repeat y x)))

(defn digit
  [d]
  (map read-string (map str (str d))))

(defn eul16
  [x y]
  (->> (kali x y)
       (digit)
       (reduce +')))

(defn faktorial
  [i]
  (loop [n i
         res 1]
    (if (= n 1)
      res
      (recur (dec n)
             (*' res n)))))

(defn eul20
  [x]
  (->> (faktorial x)
       (digit)
       (reduce +')))

(defn eul6
  [x]
  (- (->> (range (+ x 1))
          (reduce +)
          (kali))
     (->> (range (+ x 1))
          (map kali)
          (reduce +))))

(defn eul1
  [x]
  (- (+ (reduce + (range 0 x 3))
        (reduce + (range 0 x 5)))
     (reduce + (range 0 x 15))))
