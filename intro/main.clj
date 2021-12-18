#!/usr/bin/clojure

;; () is called a form

(defn foo[a b c d](println (format "foo %s,%s,%s,%s" a b c d)))

(def bar(fn[a b c d](println (format "bar %s,%s,%s,%s" a b c d))))

(foo 1 2 3 4)
(bar 1 2 3 4)

(def m1 { 1 "one" 
          2 "two"
          3 "three"
          4 "four"})

(def m2 (assoc {} 1 "one"
                  2 "two"
                  3 "three"
                  4 "four"))

(if (= m1 m2) 
    (println "maps are equal") 
    (println "maps are not equal"))

(if (not= m1 m2)
    (println "maps are not equal") 
    (println "maps are equal"))

(println (format "size of map => %d" (count m1)))

(def m3 (assoc {} :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :i 8 :j 9))
(println (dissoc m3 :i))
(println (dissoc m3 :g :i :j))

(defn sum-from-to_inc [a b]
  (loop [acc 0 a' a]
    (if (= a' (+ b 1)) acc (recur (+ a' acc) (inc a')))))

(defn sum-from-to [a b]
  (loop [acc 0 a' a]
    (if (= a' b) acc (recur (+ a' acc) (inc a')))))

;; example
(defn make-adder[n] (fn [x] (+ n x)))
(def add5 (make-adder 5))
(printf "%d" (add5 10))

(println (sum-from-to_inc 10 10))   ; => 10
(println (sum-from-to 10 10))   ; => 0

(println (let [x 1 y 10 z 100] [(+ x y z) (- x y z) (* x y z) (/ x y z)]))

(doseq [i (range 10)] (println (format "%s" i)))

;; (defn find-max [& xs] (let [n (dec (count xs))] (nth (sort xs) n)))

(defn read[] (let [console (. System console)] (.readLine console)))
(defn prompt[pstr] (print pstr) (flush) (let [console (. System console)] (.readLine console)))

(:import "java.lang.Math")
(println (. Math (sqrt 2)))   ;; syntax 1 
(println (Math/sqrt 2))       ;; syntax 2

(:import "java.lang.String")
(println (.length "foobar"))

(:import "java.lang.Math")
(:import "java.lang.String")
(:import "java.lang.Boolean")
(:import "java.lang.Integer")
(:import "java.lang.Double")
(:import java.util.Date)      ;; without "" returns nil
(import java.util.Date)      ;; without "" returns class
(println (Date.))

(println (keys (ns-publics 'user)))
(println (keys (ns-interns 'user)))
(println (keys (ns-imports 'user)))
(println (keys (ns-aliases 'user)))
(println (keys (ns-map 'user)))

(println (contains? (ns-imports 'user) 'Math))

;; (println (dir clojure.string))

;; (:require "mylib.clj" :as my)
;; (println (my.swap 1 2))

(println (System/getProperty "user.dir"))

(load-file "myLib.clj")
(println (myLib1/swap_default 1 2))   ;; default public
(println (myLib1/swapa 1 2))          ;; ^:publica
(println (myLib1/swap 1 2))           ;; ^:public
;; (println (myLib1/swap' 1 2))       ;; ^:private

(def list-with-options (list 1 2 3 4))
(def list-with-options! (conj list-with-options 5))     ;; adds from the head
(println list-with-options)
(println list-with-options!)

(def vector-with-options (vector 1 2 3 4))
(def vector-with-options! (conj vector-with-options 5))   ;; adds from the tail
(println vector-with-options)
(println vector-with-options!)

(map #(+ 0 %) [1 2 3 4])
(reduce #(+ %1 %2) 0 [1 2 3 4])
(reduce #(* %1 %2) 1 [1 2 3 4])
(filter #(>= % 3) [1 2 3 4 5])

(defn **[x y](Math/pow x y))
(** 2 10)
(println (map #(** 2 %) (range 10)))

;; thread first
(-> 1 (+ 2)(* 10))

;; thread last
(->> 1 (+ 2)(* 10))

