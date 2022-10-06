#!/usr/bin/env -S clojure -cp "."

(ns example-ref)

(require '[clojure.pprint :refer (cl-format)])
(require '[clojure.pprint :refer (pprint)])
(require '[clojure.pprint :as pp])

(println "example using ref")

(def x 0)
(def y x)
(dotimes [_ 10] (Thread/sleep 100) (print ".") (flush) (def x (inc x)))
(println "\nx = " x)
(println "\ny = " y)

(def r1 (ref 1))
(def z (deref r1))
(println "r1 -> " r1)
(println "z = " z)

;; vec, vector
;; vec takes collection
;; vector takes elements
(= (vec '(1 2 3 4)) (vector 1 2 3 4))
(vector? [])
(vector? '())
(= (vector [1 2 3 4] [1 2 3 4]) (vec (list (list 1 2 3 4) (list 1 2 3 4))))
(= (vector [1 2 3 4] [1 2 3 4]) (vec (list (vec (list 1 2 3 4)) (vec (list 1 2 3 4)))))

(into [1 2 3 4] [5 6 7 8])
(into [1 2 3 4] '(5 6 7 8))

;; map,map?
(def m1 (map vector [:a :b :c :d] [:A :B :C :D]))
(def m2 (map #(identity [%1 %2]) [:a :b :c :d] [:A :B :C :D]))
(assert (= m1 m2) "(map vector [x1] [y1]) equals (map #(identity %1 %2) [x1] [y1])")
(flatten (map vector [:a :b :c :d] [:A :B :C :D]))

;; seq?, sequential?
;; seq? is interface with first, rest
;;  where first on empty -> nil, otherwise -> first element
;;  where rest on empty or one element collection -> (), otherwise -> first element
(= (first (list)) nil)
(= (first (list 1)) 1)
(= (rest (list)) (rest (list 1)))

;; ArrayMap Seq
;; HashMap NodeSeq
;; Map KeySeq
;; Map ValSeq
;; for, map, take, filter, partition LazySeq
;; Vector RSeq
;; Vector ChunkedSeq

(sequential? [])
(sequential? ())
(sequential? {})
(sequential? #{})

;; destructuring
(let [[[a _],[b _]] (vec {:a 1, :b 2})] (list a b))
(let [[[a _],[b _]] (seq {:a 1, :b 2})] (list a b))
;; (let [[[a _],[b _]] {:a 1, :b 2}] (list a b))
(let [{A :a, B :b} {:a 1, :b 2}] (list A B))

;; destructuring example
(let [[a b c d e _ f] (range 1 100)] (list f d c b a f))
(let [[a b c d e _ f & g] (range 1 100)] (list f d c b a f [g]))
(flatten (let [[a b c d e _ f & g] (range 1 100)] (list f d c b a f [g])))

(let [xs (seq (vec (range 10)))
      ys (rseq (vec (range 10)))
      zs (take 10 (repeat 1))]
  (map + xs ys zs))


;; error, must be Symbol
;; (def :a 1)
(type 'a)
;; when symbols are not quoted, they evaluate to value
;; to get value from variable which contains symbol, it needs to be evaluated
(def a 1)
(def A 'a)
(def B a)
(println A)
(println B)
(println (eval A))

(def c 2)
(println (eval 'c))
(println (eval `c))
(println '~c)
(println `~c)

;; vector access
(def v1 [1 2 3 4])
(println (v1 0))
(println (get v1 0))
(println (nth v1 0))

(println (get v1 4 :missing))
(println (nth v1 4 :missing))
(println (get v1 4 0))
(println (nth v1 4 0))

(assoc v1 0 100)
(assoc v1 4 200)  ;; OK, when at append index
;; (assoc v1 5 200)  ;; error, when index > append index

(conj v1 300)   ;; append

(update v1 0 (fn[_]0))
(update-in v1 [0] (fn[_]0))

(assert (and
  (= (quote 1) '1)
  (= (quote (quote 1)) ''1)
  (= (quote (quote (quote 1))) '''1)
  (= (quote (quote (quote (quote 1)))) ''''1)) "quotes")

;; auto unquote
(+ 1 '1)
(into '[] (range 10))
(into '{} {:a 1, :b 2, :c 3, :d 4})

;; quote
(nth ''1 0)   ;; quote
(nth ''1 1)   ;; 1
;;
(nth '''1 0)   ;; quote
(nth '''1 1)   ;; (quote 1)
;;
(nth ''''1 0)   ;; quote
(nth ''''1 1)   ;; (quote (quote 1))

(map #(str %&) (range 10) (range 10) (range 10))
(map #(str %1 "," %2 "," %&) (range 10) (range 10) (range 10))
(map #(str %1 "," %2 "," %&) (range 10) (range 10) (range 10) (range 10))

(numerator (/ 1 10))
(denominator (/ 10))
(rationalize 0.75)

(conj {} [:a 1])
(into {} [[:a 1] [:b 2]])
(update-in {:a {:b {:c {:d 1, :x 2}}}} [:a :b :c] (fn[_]1))
(update-in {:a {:b {:c {:d 1, :x 2}}}} [:a :b :c] dissoc :x)

;; destructure in fn
(map (fn[[a b]] [(+ a b) (* a b)]) (for [x (range 10) y (range 10)] [x y]))

;; reader tag
#'v1
(var v1)

(= (symbol "a") 'a)

;; slurp/spit

;; ~1
;; (unquote 1)

(def x 0)
(for [x (range 10)] [`~x x])
(for [x (range 10)] [`~x 'x])
(for [x (range 10)] [`~x `x])

(for [x (range 10)] (eval [`~x x]))
(for [x (range 10)] (eval [`~x 'x]))
(for [x (range 10)] (eval [`~x `x]))

(ns test)
`(max)    ;; (clojure.core/max)
`(maxi)   ;; (test/maxi)
`(max (range 10))   ;; (clojure.core/max (clojure.core/range 10))
`(max ~(range 10))   ;; (clojure.core/max (0 1 2 3 4 5 6 7 8 9))
`(max ~@(range 10))   ;; (clojure.core/max 0 1 2 3 4 5 6 7 8 9)

(defn ^"mytag" ^:myflag ^{:info "myexample"} f "mydoc" ([]0) ([_]1) ([_ _]2) ([_ _ _]2) ([_ _ _ & _]4))
(meta #'f)
(meta (var f))
(println (:doc (meta (var f))))
(println (:tag (meta (var f))))
(println (:info (meta (var f))))
(println (:myflag (meta (var f))))

(def ^"mytag" ^:myflag ^{:info "myexample"} v "mydoc" 0)
(meta #'v)
(meta (var v))
(println (:doc (meta (var v))))
(println (:tag (meta (var v))))
(println (:info (meta (var v))))
(println (:myflag (meta (var v))))

(into [] (range 10))
(into #{} (range 10))

(cond false 1 false 2 false 3 false 4 true 5)
(condp = 0, 1 "one", 2 "two", 3 "three", 4 "four", :default)
(condp (fn[x y] (= x y)) 0, 1 "one", 2 "two", 3 "three", 4 "four", :default)

(->> (range 10))
(->> (range 10) (map (fn[x](* 2 x))))
(->> (range 10) (map (fn[x](* 2 x))) (filter #(> % 10)))

(->> 1 (Math/pow 2))
(->> (range 10) (map #(Math/pow 2 %)))

(take-while (fn[x](< x 10)) (range 100))
(drop-while (fn[x](< x 10)) (range 100))

(reductions #(+ %1 %2) (range 1 10))
(reductions #(* %1 %2) (range 1 10))

(take 10 (repeat 1))
(take 10 (repeatedly (fn[]1)))
;; not working
(def cnt 0)
(defn inc-cnt[] (def cnt (int cnt)))
;; working
(def cnt (ref 0))
(dosync (ref-set cnt (inc @cnt)))
(defn inc-cnt[] (dosync (ref-set cnt (inc @cnt))))

(def a (atom 0))
(def ag (agent 0))
(def r (ref 0))
(do 
  (println "atom => " @a)
  (println "agent => " @ag)
  (println "ref => " @r))
(do 
  (println "atom => " (deref a))
  (println "agent => " (deref ag))
  (println "ref => " (deref r)))

;; fnil
((fnil (fn[x]x) 0) nil)
((fnil (fn[x]x) 0) 0)
(println "using fnil => " (map (fnil identity 0) [0 nil 1 nil 2 nil 3 nil 4 nil]))
(update-in {:a 1} [:a] inc)
;; (update-in {:a 1} [:b] inc)
(update-in {:a 1} [:b] (fnil inc 0))

(defn ^{:enable true} foo "docstring" {:example false} [x] x)
(println (:enable (meta (var foo))))
(println (:example (meta (var foo))))

(select-keys {:a 1, :b 2, :c 3, :d 4, :f 5} [:a :b])
(select-keys (vec (range 10 0 -1)) [0,1,2,3,4])

(require '[clojure.pprint :as p])
(p/pprint (destructure '[[a b] [1 2]]))

(require '[clojure.pprint :as pp :refer [pprint] :rename {pprint pri}])
(pp/pprint 1)
(pri 1)

(let [[a b & c :as d] [1 2 3 4]] (println a))
(let [[a b & c :as d] [1 2 3 4]] (println b))
(let [[a b & c :as d] [1 2 3 4]] (println c))
(let [[a b & c :as d] [1 2 3 4]] (println d))

;; fully qualified keywords
(ns test1)
(def a1 :a)
(def A1 ::a)
(ns test2)
(def a2 :a)
(def A2 ::a)
(= test1/a1 a2)
(= test1/A1 A2)

(number? 0)
(number? 0.0)
(number? 1N)
(number? 1M)
(number? (/ 1 10))
(int? (int 0))
(char? (char 0))
(string? (str))
(map? (hash-map))
(set? (hash-set))
(list? (list))
(vector? (vector))
(symbol? (symbol "a"))
(keyword? (keyword "a"))
(fn? #())
(fn? (fn[]))

(reduce #(conj %1 %2) [] (range 10))
(reduce #(conj %1 %2) '() (range 10))

(clojure.string/join \, "abcd")
(clojure.string/join \ "abcd")

;; cons returns lists
;; prepend item at list head
(cons 1 (list))
(cons 1 (vector))
(cons 1 (hash-map))
(cons 1 (hash-set))
(cons 1 {:a 1, :b 2, :c 3, :d 4})
(cons 1 {:d 1, :c 2, :b 3, :a 4})
(cons 1 #{1,2,3,4})
(cons 1 #{4,3,2,1})
;; practical cons when used with maps
(cons [:x 0] (cons [:y 5] {:a 1, :b 2, :c 3, :d 4}))

;; conj preserves the original type
(conj (conj (list) 1) 2)
(conj (conj (vector) 1) 2)
(conj (conj (hash-set) 1) 2)
(conj (conj (hash-map) [:a 1]) [:b 2])

;; lazy-seq
(defn f[x](lazy-seq (cons x (f x))))
(take 20 (f 0))
(defn f[](lazy-seq (cons (+ 1 (rand-int 100)) (f))))
(take 20 (f))

((fn[[x y]](+ x y)) [1 2])
((fn[[x y :as xy]] (println "arg" xy) (+ x y)) [1 2])

;; composition
((comp (fn[x]x) (fn[x]x)) 1)
((comp (fn[[x y]] [(inc x) (dec y)]) (fn[[x y]] [(dec x) (inc y)])) [1 1])

(take 20 (drop 20 (range)))

;; example of reducing function
(transduce (comp (filter odd?) (take 20)) 
  (fn ([acc x] (+ acc x))
      ([x] x)
      ([] 0))
  0
  (range))

(transduce (comp (filter odd?) (take 20)) 
  (fn ([acc x] (conj acc x))
      ([x] x)
      ([] 0))
  []
  (range))

(transduce (comp (filter odd?) (take 20)) 
  (fn ([acc x] (conj acc x))
      ([x] [x])
      ([] []))
  []
  (range))

