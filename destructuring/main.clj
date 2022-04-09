#!/usr/bin/env clojure

(println "in main")

;; passed vector is destructured into two components
(defn f1[[x y]] (+ x y))
(println "f1[[x y]]" (f1 [1 2]))

(defn f2[[x y :as xy]] (= (reduce #(+ %1 %2) xy) (+ x y)))
(println "f2[[x y] :as xy]" (f2 [1 2]))

(println "pair =>" (let [[a b c d _] [1 2 3 4 5]] (list (+ a b) (+ c d))))
(println "pair =>" (let [[a b c d _ :as all] [1 2 3 4 5]] (list (+ a b) (+ c d) (map #(pos? %1) all))))

;; & args != &args
(println (let [[a b c d _ & x] [1 2 3 4 5 6 7 8 9]] x))
(println "count =>" (let [[a b c d _ & x :as all] [1 2 3 4 5 6 7 8 9]] (list (count x) (count all))))

(println "map destructure with non-existing key =>" (let [{x :x} {:a 1 :b 2}] x))
(println "map destructure with existing key =>" (let [{x :x} {:a 1 :b 2 :x 3}] x))
(println "map destructure with more existings keys =>" (let [{:keys [x]} {:a 1 :b 2 :x 3}] x))
(println "map destructure with more existings keys =>" (let [{:keys [x a]} {:a 1 :b 2 :x 3}] (list x a)))
;; (println "map destructure with more existings and non-existing keys and optional =>" (let [{:keys [x a b c], :or {:c 100}} {:a 1 :b 2 :x 3}] (list x a)))

(println "map destructure with or (x taken from or) =>" (let [{x :x, :or {x 10}} {:a 1 :b 2}] x))
(println "map destructure with or (x taken from param) =>" (let [{x :x, :or {x 10}} {:a 1 :b 2 :x 100}] x))
(println "map destructure with keys, or (x taken from or) =>" (let [{:keys [x], :or {x 10}} {:a 1 :b 2}] x))
(println "map destructure with keys, or (x taken from param) =>" (let [{:keys [x], :or {x 10}} {:a 1 :b 2 :x 100}] x))
(println "map destructure with keys, or, as (x taken from or) =>" (let [{:keys [x a], :or {x 10}, :as all} {:a 1 :b 2}] x))
(println "map destructure with keys, or, as (x taken from param) =>" (let [{:keys [x a], :or {x 10}, :as all} {:a 1 :b 2 :x 100}] x))

;; all includes only the param keys, y is taken from or
(println "map destructure with keys, or, as ... all =>" (let [{:keys [x y a], :or {x 10 y 1000}, :as all} {:a 1 :b 2 :x 100}] (list all y)))




