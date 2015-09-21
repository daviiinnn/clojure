# clojure
cobacoba


(ns asd.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn trim [n]
  (case n
    0 0
    1 1
    2 1
    3 1
    10 105
    20 46499
    30 20603361
    35 433695873
    40 9129195487

    (+ (trim (- n 1))
       (trim (- n 2))
       (trim (- n 3)))))

;; sumpah ini ngentot!!!
;; trim 1 => 1
;; trim 2 => 1
;; trim 3 => 1
;; trim 4 => 3
;; trim 5 => 5
;; trim 6 => 9
;; trim 7 => 17
;; trim 8 => 31
;;mampus gw udah bisa





(def fibo (lazy-cat [1 1 1] (map +' fibo (rest fibo) (rest (rest fibo)))))


(def fi (lazy-cat [0 1] (map +' fi (rest fi))))







(defn pa
  [x res]
  (loop [n x
         t res]
    (if (zero? n)
      t
      (recur (quot n 10) (conj t (rem n 10))))))


(defn fib [n]
  (case n
    0 0
    1 1
    2 1
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn kali

  [x y]
  (apply * (repeat y x)))

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(def prime (sieve (iterate inc 2)))

(defn gra
  [nilai]
  (+ (* 34 (kali nilai 16)) (* 43 (kali nilai 13)) (* 23 (kali nilai 12)) (* 20 (kali nilai 10)) (* 12 (kali nilai 9)) (* 234 (kali nilai 5)) (* 34 (kali nilai 3)) (* 54 (kali nilai 2))))


(def cai
  (count (apply concat (map #(pa % []) (range 1 12345678987654321)))))

(defn sum
  [x]
  (if (<= x 1)
    1
    ((+ x (sum (dec x))))))

(defn facto
  [x]
  (if (<= x 1)
    1
    (*' x (facto (dec x)))))

(def anje
  (map #(sum %) (range 1 1000)))

(defn coba
  [x]
  (loop [y x]
    (if (<= y 1)
      1
      (recur (- x 10)))))

