#!/usr/bin/env -S clojure -cp "."

;; dispatcher, keyword used as function
(defmulti dispatcher :shape)
(defmethod dispatcher "circle" [p] (do (println "circle dispatcher") (keys p)))
(defmethod dispatcher "square" [p] (do (println "square dispatcher") (keys p)))
(defmethod dispatcher "rect" [p] (do (println "rect dispatcher") (keys p)))
(defmethod dispatcher :default [p] (do (println "default dispatcher") (keys p)))

(dispatcher {:shape "circle", :radius 10})
(dispatcher {:shape "square", :side 10})
(dispatcher {:shape "rect", :side1 10, :side2 20})
(dispatcher {:shape "else"})

;; area, keyword used as function
(defmulti area :shape)
(defmethod area "circle" [p] (let [r (:radius p)] (* Math/PI r r)))
(defmethod area "square" [p] (let [x (:side p)] (* x x)))
(defmethod area "rect" [p] (let [x (:side1 p), y (:side2 p)] (* x y)))
(defmethod area :default [p] 0)

(do
  (println 
  (area {:shape "circle", :radius 10})
  (area {:shape "square", :side 10})
  (area {:shape "rect", :side1 10, :side2 20})
  (area {:shape "else"})))

;; fac, identity as function
(defmulti fac identity)
(defmethod fac 0 [_] 1)
(defmethod fac :default [n] (* n (fac (dec n))))

(do
  (println (fac 0))
  (println (fac 1))
  (println (fac 2))
  (println (fac 3))
  (println (fac 4))
  (println (for [i (range 10)] (fac i)))
  (println (map fac (range 10))))

;; resolver function
;; (def resolver-function (fn[x] (#(:pin_type %) x)))
(def resolver-function (fn[x] (:pin_type x)))
(defmulti match resolver-function)
;; (defmulti match :pin_type)
(defmethod match :digital_io [pin] "dig io pin")
(defmethod match :analog_io [pin] "ana io pin")
(defmethod match :power [pin] "power pin")

(do 
  (println (match {:pin_type :power, :pin_function :SUPPLY, :volt :P5V}))
  (println (match {:pin_type :power, :pin_function :GND}))
  (println (match {:pin_type :digital_io, :pin_function :INPUT, :tolerance :FVT}))
  (println (match {:pin_type :digital_io, :pin_function :OUTPUT, :output_type :PUSH_PULL}))
  (println (match {:pin_type :digital_io, :pin_function :OUTPUT, :output_type :OPEN_DRAIN}))) 


;; meta
(defmulti ^{:doc "dispatching on :shape keyword"} area :shape)
(println "reading doc from defmulti => " (:doc (meta (var area))))

;; misc
(def f (fn[x]x))
(def f (fn[x](* 2 x)))
(def q println)
(loop [acc 0, i 10] 
  (q i)
  (if (zero? i) 
      acc 
      (recur (+ acc (f i)) (dec i))))

;; test
(defn ^{:test #(do (printf "running test") (assert true))} foo[_] 1)
(test #'foo)
(println)

;; (defn ^{:test #(do (printf "running test") (assert false))} bar[_] 1)
;; (test #'bar)
;; (println)

(defn 
  ^{:test #(do 
      (assert (= (sums 0) 0))
      (assert (= (sums 1) 1))
      ;; (assert (= (sums 2) 2))
      (assert (= (sums 10) 55)))} 
  sums[n] (loop [acc 0, i n] (if (zero? i) acc (recur (+ acc i) (dec i)))))




