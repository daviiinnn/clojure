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





(defn kali
  [x y]
  (apply *' (repeat y x)))


(def trimo (lazy-cat [1 1 1] (map +' trimo (rest trimo) (rest (rest trimo)))))


(def fi (lazy-cat [0 1] (map +' fi (rest fi))))


(defn pa
  [x g]
  (loop [n x
         t []]
    (if (zero? n)
      (if (zero? x)
        [0]
        t)
      (recur (quot n g) (cons (mod n g) t)))))

(defn fib [n]
  (case n
    0 0
    1 1
    2 1
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(def prime (sieve (iterate inc 2)))

(defn primah
  [x]
  (last (take x prime)))

(defn gra
  [nilai]
  (+ (* 34 (kali nilai 16)) (* 43 (kali nilai 13)) (* 23 (kali nilai 12)) (* 20 (kali nilai 10)) (* 12 (kali nilai 9)) (* 234 (kali nilai 5)) (* 34 (kali nilai 3)) (* 54 (kali nilai 2))))4




(defn sum
  [x]
  (if (<= x 1)
    1
    (+' x (sum (dec x)))))
(defn sumsu
  [x]
  (loop [l x
         has 0]
    (if (= l 0)
      has
      (recur (dec l) (+ l has)))))

(defn facto
  [x]
  (if (<= x 1)
    1
    (*' x (facto (dec x)))))

(def anje
  (map #(sum %) (range 1 1000)))


(defn sumloop
  [x]
  (loop [lst x
         s 0]
    (if (= lst '())
      s
      (recur (rest lst)
             (+ s (first lst ))))))

(defn faktorial
  [i]
  (loop [n i
         res 1]
    (if (= n 1)
      res
      (recur (dec n)
             (*' res n)))))

(defn kentoi
  [x y]
  (loop [runi x
         aw y
         frist []]
    (if (= aw (- y 10))
      frist
      (recur (*' runi aw ) (dec aw) (conj frist (*' runi aw)) ))))

(defn sasan
  [x]
  (loop [suu 0
         pol 1
         b []]
    (if (= pol (+ x 1))
      b
      (recur (+ suu pol) (inc pol) (conj b (+ suu pol)))
      )))

(defn kentoa
  [x]
  (loop [hy 1
         huhu 2
         j '()]
    (if (= huhu (+ x 2))
      j
      (recur (sumsu huhu) (inc huhu) (cons hy j)))))


;; [[34,26],[12,25],[45,24],[43,23],[23,22],[20,20],[12,19],[32,18],[43,16],[234,15],[34,13],[54,12], [123,10],[321,9]]
;;[[655357614912000,16],[142340115456000,15],[320265259776000,14],[178518228134400,13],[53970627110400,12],[13408851456000,10],[4022655436800,9],[5081248972800,8],[1249555507200,6],[2549965017600,5],[35286451200,3],[12933043200,2]]

(defn bangsat
  [x y]
  (last (kentoi x y)))

(def turunan
  (map #(bangsat % %2) [34 26] [12 25] [45 24]))






(defn srow
  [x]
  (loop [pa 30
         s '()]
    (if (= pa x)
      s
      (recur (inc pa) (conj s (apply + (map #(primah %) (range (+ 1 (first (kentoa pa))) (+ 1 (first (kentoa (inc pa))))))))))))

(defn modulo
  [x]
  (loop [ro 1
         p '()]
    (if (= ro x)
      p
      (recur (inc ro) (conj p (mod ro 100))))))

(defn digit
  [x]
  (apply concat (reverse (digit1 x))))

(defn primes-to
  [n]
  (let [root (-> n Math/sqrt long),
        cmpsts (boolean-array (inc n)),
        cullp (fn [p]
                (loop [i (* p p)]
                  (if (<= i n)
                    (do (aset cmpsts i true)
                        (recur (+ i p))))))]
    (do (dorun (map #(cullp %) (filter #(not (aget cmpsts %))
                                       (range 2 (inc root)))))
        (filter #(not (aget cmpsts %)) (range 2 (inc n))))))



(defn gcd [x y]
  (cond
    (zero? x) y
    (zero? y) x
    :else (recur y (mod x y))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn prime? [n]
  (= 2 (reduce +
               (for [i (range 1 (inc n))]
                 (if (= 0 (mod n i)) 1 0)))))


(defn palindrome? [g]
  (= (reverse (str g) ) (seq (str g))))





(defn iter-prime [x y]
  (cond (= y x) true
        (zero? (rem x y)) false
        :else (iter-prime x (inc y))))





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

(defn primahh
  [o]
  (let [primx (fn primx
                [x]
                (let [akar (Math/sqrt x)
                      iter (fn iter [i]
                             (cond (> i akar) true
                                   (zero? (rem x i)) false
                                   :else (iter (+ i 2))))]
                  (cond (<= x 1) false
                        (= x 2) true
                        (even? x) false

                        :else (iter 3))))]
    (take o (filter primx (range 1000)))))



(defn cobalcm
  [x y]
  (let [gcd (fn gcd [b c]
              (cond
                (zero? b) c
                (zero? c) b
                :else (recur c (mod b c))))]
    (/ (* x y) (gcd x y))))



(defn kepascal
  [x]
  (let [pascar (iterate #(concat '(1)
                                 (map + % (rest %))
                                 '(1) )'(1))]
    (last (take x pascar))))


(defn kayu
  [x]
  (let [digit (fn digit
                [d]
                (map read-string (map str (str d))))]
    (if (= (->> (digit x)
                (take (/ (count (digit x) )2))
                (reduce +))
           (->> (reverse (digit x))
                (take (/ (count (digit x) )2))
                (reduce +)))
      true
      false)))

(defn nama-asli
  [nama]
  (str nama "TOT!"))









