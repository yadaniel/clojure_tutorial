#!/usr/bin/env clojure

(ns types)

(println "(instance? java.lang.Object 1) => " 
        (instance? java.lang.Object 1))

(println "(instance? java.lang.Object 1.0) => " 
        (instance? java.lang.Object 1.0))

(->> [true false 0 0.0 [] '()]
     (map #(instance? java.lang.Object %)))

(def v (->> [true false 0 0.0 [] '()]
     (map #(instance? java.lang.Object %))))

(println v)

;; lazy sequence
(def a (lazy-seq [1 2 3 4]))
(prn "type a => " (type a))

(def a' (map #(identity %) [1 2 3 4]))
(prn "type a' => " (type a'))

(def a'' (filter #(< % 10) (range 100)))
(prn "type a'' => " (type a''))

(= (cons 1 '()) (cons 1 []))
(= (list* 1 2 3 4 '()) (list* 1 2 3 4 []))

;; let f = x : f (x + 1)
(defn foo[x] (lazy-seq (cons x (foo (inc x)))))
(take 10 (foo 0))
(defn foo'[x] (lazy-seq (list* x x (foo' (inc x)))))
(take 10 (foo' 0))

(defn fibs[a b] (lazy-seq (cons a (fibs b (+ a b)))))
(prn (take 100 (fibs 1N 1)))

;; wrapped
(defn fibs''[] ((fn fibs'[a b] (lazy-seq (cons a (fibs' b (+ a b))))) 1N 1))
(prn (take 100 (fibs'')))

;; let fibs = 1:1: zipWith (+) fibs (tail fibs)
(defn f[] (let [xs '()] (loop [a 1 b 1] (cons a xs) (recur b (+ a b)))))

(println (take 10 (repeatedly #(rand-int 100))))

