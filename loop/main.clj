#!/usr/bin/env clojure

(ns main)

(loop [i 100 x 0] (if (zero? i) x (recur (dec i) (+ x i))))
(println (loop [i 100 x 0] (if (zero? i) x (recur (dec i) (+ x i)))))

(def strarr (.split "foo/bar/foobar" "/"))
(type strarr)
(println (vec strarr))

(println (vector
  (-> 10 (range))       ;; thread first
  (->> 10 (range))      ;; thread last
  (comment this is comment form)
  (-> 10 (range 0)) 
  (->> 10 (range 0))
))

;; ^Long
;; ^Short
;; ^Float
;; ^Boolean
;; ^Character
(defn b "f takes x:byte and returns x" [^Byte x] x)
(defn f "f takes x:int and returns 2x" [^Integer x] (* 2 x))
(defn p "p takes x:double and returns -2x" [^Double x] (- (* 2 x)))
(defn q "q takes x:string and returns uppercase" [^String x] (.toUpperCase x))
(println (q "foo"))
(meta f)

(macroexpand '(when true 1))
(Integer/MIN_VALUE)
(Integer/MAX_VALUE)

(Double/MAX_VALUE)
(Double/MIN_VALUE)

(Byte/MIN_VALUE)  ;; -128
(Byte/MAX_VALUE)  ;; 127

;; , equals space
(defn,one"description of one"[]1)

(import java.lang.String)
(import java.lang.Math)
(Math/sin 0)

;; keys, vals
({1 100 2 200 3 300 4 400} 1)     ;; OK
;; (1 {1 100 2 200 3 300 4 400})

(type #"")
(re-find #"\d+" "a1234")      ;; 1234
(re-matches #"\d+" "a1234")   ;; nil
(count (re-seq #"\d+" "123 456 789"))
(count [1 2 3 4])
(count '(1 2 3 4))

(def x (for [i (seq '(1 2 3 4))] (do (println i) (* 2 i))))
(def x' (for [i (seq [1 2 3 4])] (do (println i) (* 2 i))))
(def x'' (for [[k,v] (seq {1 2, 3 4})] (do (println k) (* 2 v))))
(println x x' x'')

(def z (conj (conj (conj (conj [] 1) 2) 3) 4))
(def z' (conj (conj (conj (conj [])))))
(println z z')

(type #_(map) 1)


