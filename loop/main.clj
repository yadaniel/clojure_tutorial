#!/usr/bin/env clojure

(ns main)

;; list comprehension
;; for
;; (for [i (range 10)] i)
;; (for [i (range 10) j (range 10)] (+ i j))
;; (for [i (range 10) j (range 10) :when true] (+ i j))
;; (for [i (range 10) j (range 10) :when (= 15 (+ i j))] (list i j))
;; (for [i (range 10) j (range 10) :let [z (+ i j)] :when (= 15 z)] (list i j))
;; (for [i (range 10) j (range 10) :let [z (+ i j) p (* i j)] :when (= 15 z)] (list i j p))
;; general structure
;; (for [i (range 10) j (range 10) :when true :when true :when true] (+ i j))
;; (for [i (range 10) j (range 10) :let [] :when true :when true :when true :let []] (+ i j))

;; dotimes
;; (dotimes [i 10] (println i))

;; loop
;; (loop [i 0] (do (println i) (if (< i 10) (recur (inc i)))))

;; ;; let
;; (let[])
;; (let[] (let[]))
;; (let[] (let[](let[])))
;; (let[i 0] (let[i 1](let[i 2])))
;; (let[i 0] (let[i 1] (let[i 2] (println "level 2 =>" i)) (println "level 1 =>" i)) (println "level 0 =>" i))
;; (let[i 0] 
;;   (let[i 1] 
;;     (let[i 2] 
;;       (println "level 2 =>" i)) 
;;     (println "level 1 =>" i)) 
;;   (println "level 0 =>" i))

(def q [])
(dotimes [i 10] (def q (conj q i)))
(println "q =" q)

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

(for [i '(map filter reduce)] (type (eval i)))
(for [i [1 2 3 4] j [10 20 30 40]] (+ i j))

(println "take 10 repeat 1 =>" (take 10 (repeat 1)))

(list)
(vector)
(hash-map)

;; only in REPL
;; (doc map)

(string? "")
(integer? 1)
(map? {})
(list? '())
(vector? [])
(seq? '())

(def a 10)
(ns foo)
(def a 1)
(def b 1)
(def c 1)
;; (println a)
(println foo/a)
(refer 'foo)
(println a)
(assert (= a foo/a))
(assert (= a 1) "a shall be 1")
(ns main)
(assert (= a main/a))
(assert (= a 10) "a shall be 10")

;; (try (/ 1 0) 
;;      (catch Exception e (do (println "catched") (println (.getMessage e))))
;;      (finally (println "finally")))

;; Throwable.toString()
;; Throwable.getMessage()
;; Throwable.getStackTrace()
(try (/ 1 0) 
     (catch ArithmeticException e (do (println "arithmetic exception") (println (.toString e))))
     (catch Exception e (println "exception")) 
     (finally (println "finally")))

;; (try (/ 1 0) 
;;      (catch ArithmeticException e (do (println "arithmetic exception") (println (.getMessage e))))
;;      (catch Exception e (println "exception")) 
;;      (finally (println "finally")))

;; (try (/ 1 0) 
;;      (catch ArithmeticException e (println "arithmetic exception"))
;;      (catch Exception e (println "exception")) 
;;      (finally (println "finally")))


