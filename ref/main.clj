#!/usr/bin/env -S clojure -cp "."

(ns example-ref)

;; warning
;; (require '[clojure.core :as c :refer [reverse]])
;; (require '[clojure.string :as s :refer [reverse]])
;; error
;; (require '[clojure.set :as s])
;; (require '[clojure.string :as s])

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
(def vi0 (vector-of :int))
(def vi1 (vector-of :int 1 2 3 4))

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
(conj [] 1)
(conj [] 1 2)
(conj [] 1 2 3)
(conj [] 1 2 3 4)

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
(type #'v1)   ;; Var

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
(println (Math/sin 0))
(println (Math/asin 0))
(println (Math/cos 0))
(println (Math/acos 0))
(println (Math/tan 0))
(println (Math/atan 0))
(println (Math/sinh 0))
(println (Math/cosh 0))
(println (Math/tanh 0))
(println (list Math/PI Math/E))
(println (Math/sqrt 4))
(println (Math/floor 1.1))
(println (Math/ceil 1.1))
(println (Math/abs -1.1))
(println (Math/log10 1))
(println (Math/log Math/E))
(println (Math/random))
(println (clojure.core/rand))
(println (clojure.core/rand 10))
(min 1 2 3 4 0)
(max 1 2 3 4 0)
(clojure.core/min 1 2 3 4 0)
(clojure.core/max 1 2 3 4 0)

(take-while (fn[x](< x 10)) (range 100))
(drop-while (fn[x](< x 10)) (range 100))

(reductions #(+ %1 %2) (range 1 10))
(reductions #(* %1 %2) (range 1 10))
(reductions (fn[acc x](+ acc x)) -50 (take 100 (repeat 1)))

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

;; ignore acc, provide initial and empty collection
(reduce (fn[acc x]x) 0 [])
(reduce (fn[acc x]x) 1 [])
(defn f[] (lazy-seq (cons 1 (f))))
(take 20 (f))
(defn f[x] (lazy-seq (cons x (f x))))
(take 20 (f 1))
(defn f[x] (lazy-seq (cons x (f (inc x)))))
(take 20 (f 0))

(take 20 (repeatedly (fn[]1)))
(take 20 (repeatedly 10 (fn[]1)))
(drop 20 (repeatedly 80 (fn[](rand-int 100))))
(take-while (fn[x](not= x 0)) (repeatedly (fn[](rand-int 100))))
(drop-while (fn[x] (< x 10)) (range 100))   ;; will drop 0..9
(drop-while (fn[x] (< x 10)) (cons 10 (range 100)))   ;; will drop nothing
(take-while (fn[x] (< x 10)) (range 100))   ;; will take 0..9
(take-while (fn[x] (< x 10)) (cons 10 (range 100)))   ;; will take nothing

;; ((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9))
(partition 2 (range 10))    ;; in chunks of size 2
(partition 2 1 (range 10))  ;; default step is 1
;; ((0 1) (2 3) (4 5) (6 7) (8 9))
(partition 2 2 (range 10))  ;; in chunks of 2 with step 2 (from subseq to subseq)

;; zipping elements
(map (fn[x y z][x y z]) (range)(range)(range 10))
(map (fn[x y][(+ x y)(* x y)]) (range)(range 10))

;; map for vector, efficient version
(type (mapv (fn[x]x) []))   ;; PersistentVector
(type (map (fn[x]x) []))    ;; LazySeq
;; given sequences are provided as arguments
(type (mapv (fn[x y](+ x y)) [0] [0]))
(type (map (fn[x y](+ x y)) [0] [0]))
;; usage
(println (mapv (fn[x y](+ x y)) [] []))           ;; works with empty vectors
(println (mapv (fn[x y](+ x y) (* x y)) [0 1 2 3] [4 5 6 7 8]))   ;; addition resuls is skipped
(println (mapv (fn[x y] [(+ x y) (* x y)] ) [0 1 2 3] [4 5 6 7 8]))   ;; lower sequences defines the length
;;
(println (mapv (fn[[x y]] (+ x y)) [[1 1]]))
(println (mapv (fn[[x y :as xy]] (+ x y)) [[1 1]]))
(println (mapv (fn[[x y] [a b]] [(+ x a) (* y b)]) [[1 1]] [[2 2]]))
(println (mapv (fn[[x y :as xy] [a b :as ab]] [(+ x a) (* y b)]) [[1 1]] [[2 2]]))

(hash-map :A 1, :B 2)       ;; keys and vals
(into {} [[:A 1], [:B 2]])  ;; vector of keys and vals
(zipmap [:A :B] [1 2])      ;; keys and vals separate

;; read from console and type convert it
;; 1 => Long
;; 1.1 => Double
;; 1/3 => Ratio
;; foo => Symbol
;; :foo => Keyword
;; "foo" => String
(do
  (print "enter: ")
  (flush)
  (def q (read)))

;; common functions from string namespace
(clojure.string/split-lines "foo\nbar\nfoorbar")
(clojure.string/split "foo\nbar\nfoobar" #"\n")
(clojure.string/split "foo;bar;foobar" #";")
;; without full namespace
(require '[clojure.string :as string])
(string/split-lines "foo\nbar\nfoorbar")
(string/split "foo\nbar\nfoobar" #"\n")
(string/split "foo;bar;foobar" #";")
;; refer directly in this namespace
(require '[clojure.string :as string :refer [split-lines, split]])
(split-lines "foo\nbar\nfoorbar")
(split "foo\nbar\nfoobar" #"\n")
(split "foo;bar;foobar" #";")

;; further string functions
(require '[clojure.string :as string])
(string/join \: ["foo" "bar" "foobar"])
(string/replace "abcde" "e" "")
(string/replace "abcde" "e" "ee")
(string/replace "foO" #"[oO]" "a")
(string/replace "foO" #"(?i)o" "a")
(string/replace "foO" #"(?i)O" "a")
(string/replace "var    =   100" #"(\w+)\s+=\s+(\d+)" "$2 = $1")
(string/trim " abcde ")
(string/triml " abcde ")
(string/trimr " abcde ")
(string/trim-newline "\nline1\nline2\nline3\nline4\n")  ;; remove the last newline
(count " abcde ")
(.length " abcde ")
(. " abcde " length)
(def s0 (String.))
(def s1 (String. ""))
(def s2 (String. "f"))
(.concat (.concat s0 "foo") "bar")

(defn remove-non-printable-characters [x]
  (clojure.string/replace x #"\p{C}" ""))

(require '[clojure.java.io :as io])
(def r (io/reader "data"))
(. r read)

;; read from file
(defn char-stream[reader] (lazy-seq (cons (.read reader) (char-stream reader))))
(def data-reader (io/reader "data"))
(def txt-chars (map char (take-while #(not= -1 %) (char-stream data-reader))))
(def txt-str (apply str txt-chars))
;; simple
(def txt (slurp "data"))
(println "txt-str == txt => " (= txt-str txt))

(dotimes [_ 10] (Thread/sleep 1000) (println ((clojure.string/split (str (java.util.Date.)) #" ") 3)))

(def m (group-by #(> % 10) (range 20)))
(m true)
(m false)

(map #(let[[a b]%](format "%s %s" a b)) [[1 2]])
(map #(format "%s %s" (first %) (second %)) [[1 2]])
(map #(format "%s %s" (% 0) (% 1)) [[1 2]])
(map #(format "%s %s" (nth % 0) (nth % 1)) [[1 2]])

;; using apply
;; arguments to apply must be within a vector or list
(apply + [1 2 3 4])
(apply (fn[a b c d] (+ a b c d)) [1 2 3 4])
(apply (fn[a b c d] (+ a b c d)) '(1 2 3 4))

;; using subvec => inclusive index, exclusive index => returns vector
(subvec (vec (range 10)) 1)     ;; from 1 to end of sequence
(subvec (vec (range 10)) 1 2)   ;; from inclusive 1 to exclusive 2
(subvec (vec (range 10)) 1 1)   ;; empty
;; (subvec (vec (range 10)) 1 11)  ;; exception index-out-of-bounds

;; there is no substr function
;; note, using mapv will map and convert result to vector
;; subvec requires vector and will not work with list
(defn substr
  ([s, i](apply str (subvec (mapv char s) i)))
  ([s, i, j](apply str (subvec (mapv char s) i j))))

;; divmod
(defn divmod[x y] [(quot x y) (mod x y)])
(format "%s" (range 10))
(format "%s" (map (fn[x]x) (range 10)))
(format "%s" (map identity (range 10)))
(format "%s" (mapv (fn[x]x) (range 10)))    ;; convert to vector, evaluates lazy seq
(format "%s" (apply str (map (fn[x]x) (range 10))))   ;; apply str evaluates lazy seq
(format "%s" (apply str (map (fn[x] (str x " ")) (range 10))))   ;; apply str evaluates lazy seq
(format "%s" (string/join "," (map (fn[x]x) (range 10))))

(->> 1 (+ 2) (* 10))
(->> 1 (+ 2) (* 10) (#(cons % ())))
(->> 1 (+ 2) (* 10) (#(conj () %)))

(map key {:a 1, :b 2})
(key (clojure.lang.MapEntry. :a :b))  ;; returns :a
(conj {} (clojure.lang.MapEntry. :a :b))
(into {} (map (fn[x y][(keyword (str "X" x)) y]) (range 10) (range 10)))

(.toLowerCase "FOO")
(.toUpperCase "foo")

(clojure.string/lower-case "FOO")
(clojure.string/upper-case "foo")

;; forward declaration => unbound
(def forward_x)
(intern 'user 'forward_y)

(require 'clojure.inspector)
(clojure.inspector/atom? (atom 1))
(clojure.inspector/atom? (ref 1))

(def f (comp + +))
(f 1 1)
(def f (comp + + +))
(f 1 1)
(def f (comp inc inc inc inc))
(f 0)
(def f (comp (fn[x](+ 10 x)) (fn[x](* 2 x))))   ;; from right to left
(f 0)

(concat [][][][])
(concat [1][2 3][4 5 6][7 8 9 10])
(concat '(1) '(2 3))
(concat '(1) '(2 3) [4])
(concat {:foo 1} {:bar 2} {})
(into {} (concat {:foo 1} {:bar 2} {}))
(concat #{1 2 3} #{4 5 6} #{1 7 10})
(concat #{1 2 3} #{4 5 6} #{1 7 10} [1] '(1 0))

(apply str (interpose \_ "Hello"))
(interpose 0 [1 2 3 4])
(list* '(1 2 3 4))
(shuffle (range 10))
(map (fn[x](apply + x)) (for[_ (range 10)] (shuffle (range 10))))
(some #{1 2 3 4} [1 2 3 4])

(Integer/toBinaryString 15)
(Integer/toHexString 15)
(Integer/toOctalString 15)

;; general use
(Integer/parseInt "10")
(Integer/parseInt "10" 10)
(Integer/parseInt "10" 2)
(Integer/parseInt "10" 16)

;; use for hex, oct, dec base, base is implicit
(Integer/decode "0x10")
(Integer/decode "010")
(Integer/decode "10")

(string/join \, "foobar")
(string/join \, (map char "foobar"))
(apply str (interpose \, "foobar"))

((partial (fn[x y](+ x y)) 10) 20)
(def p (partial (fn[x y](+ x y)) 10))
(defn q[y] (p y))

;; parition, arg1 = size of partition, arg2 = increment between partitions
;; not full partitions are skipped
(= (partition 2 (range 10)) (partition 2 (range 11)))   ;; floor(11/2)
(= (partition 2 (range 10)) (partition 2 2 (range 10)))
(= (flatten (partition 1 (range 10))) (range 10))
(partition 1 (range 10))    ;; list of lists of one element
(partition 3 (range 10))

;; flatten, apply concat
(= (apply concat [[1] [2]]) (flatten [[1] [2]]))
(= (apply concat (apply concat [[[1]] [[2]]])) (flatten [[[1]] [[2]]]))

(= Double/NaN Double/NaN)
(= Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY)
(= Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY)
(- Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY)
(- Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY)

;; bit wise
(def kb (bit-shift-left 1 10))
(def mb (bit-shift-left 1 20))
(= (bit-shift-right 16 4) 1)
(= (bit-or 1 2 4 8 16) 31)
(= (bit-and 1 2 4 8 16) 0)
(bit-xor 0 1)
(bit-not 0)   ;; -1, type Long
(defn bit-identity'[x] (bit-xor (bit-xor x 0xFFFF) 0xFFFF))
(defn bit-identity''[x] (bit-xor (bit-xor x 0x0000) 0x0000))

;; java interop Character
(Character/isWhitespace \a)
(Character/isLowerCase \a)
(Character/isUpperCase \a)
(Character/isDigit \1)
(Character/isLetter \a)
(Character/isISOControl \a)
(Character/isLetterOrDigit \a)
(map #(Character/isWhitespace %) "foo bar\tfoobar\n")

;; record
(defrecord point3d [x1 x2 x3])
(def origin3d (point3d. 0 0 0))
(:x1 origin3d)
(-> origin3d :x1)
(get origin3d :x1)

;; (+ (read) (read))

;; java array
(require '[clojure.pprint :as pp :refer [pprint]])
(def l (make-array Long 10))
(pprint l)
(aset l 0 1)
(pprint l)
(println (count l))

(def d5 (make-array Double 5))
(def d10 (make-array Double/TYPE 10))
(def d10x10 (make-array Double/TYPE 10 10))

;; repl
(require '[clojure.pprint :as pp :refer [pprint]])
(clojure.pprint/pprint 1)
(pp/pprint 1)
(pprint 1)
(resolve 'pprint)
;; (clojure.main/repl :print :pprint)   ;; uncomment to drop to repl

;; will require and refer all functions
(use 'clojure.pprint)
(use 'clojure.string)   ;; warning => clojure.core/revere, clojure.core/replace replaced

(let [[a b c d] [1 2 3 4]] a)
(let [[a b c d] (range 10)] a)
(let [[a b c d & e] (range 10)] a)
(let [[a b c d & e] (range 10)] e)

(let [] 1)
(fn [] 1)
((fn [] 1))

((fn [[]] 1) 2)
((fn [{}] 1) 2)

((fn [[a b c d]] a) [1 2 3 4])
((fn [{a :a}] a) {:a 1})
((fn [{a :a, b :b}] a) {:a 1, :b 2})
((fn [{a :a, b :b, :as xs}] xs) {:a 1, :b 2})

((fn[& x](nth x 0)) 1 2 3 4)
((fn[& x](nth x 0)) [1 2 3 4])

(interleave (range 10) (map char (range 48 100)))
(interleave (range 10) (map char (range 48 100)) [:foo :bar :foobar])

(System/getenv "PATH")
(def e (System/getenv))
(def e' (into {} e))
(e' "PATH")

(apply vector (range 10))
(vector 1 2 3 4 5 6 7 8 9)
(vec (range 10))

;; (import java.util.jar.JarFile)
;; (def f (JarFile. "./A.jar"))
;; (while (.hasMoreElements entries)
;;     (let [entry (.nextElement entries)]
;;         (println (.getName entry))))

;; \n = \r\n, but not \n\r or \r
(require '[clojure.string :as str])
(=
  (str/split-lines "foo\nbar")
  (str/split-lines "foo\r\nbar"))

(import javax.swing.JFrame)
(import '[java.util Date UUID Random Locale Timer OptionalInt Calendar Base64])
(import '[java.time Clock LocalTime LocalDate Year Instant MonthDay])

(.nextInt (Random.))
(def r (Random.))
(.setSeed r 0)
(.nextLong r)

(.nextDouble (doto (Random.) (.setSeed 0) (.nextLong)))
(.nextDouble (doto (Random.) (.setSeed 0)))

(def bx (byte-array 10))
(pprint bx)
(.nextBytes r bx)
(pprint bx)

(while (< (.nextInt r) 1000000) (pprint "here"))

(let [r (doto (Random.) (.setSeed 0)), v (atom 0)]
  (while (do
           (reset! v (mod (.nextInt r) 100))
           (< @v 80))
    (pprint (str "value was " @v))))

(.millis (Clock/systemUTC))

;; (let [ms #(.millis (Clock/systemUTC)), now (ms), stop (+ 60000 now)]
(let [ms #(.millis (Clock/systemUTC)), now (ms), stop (+ 6000 now)]
  (while (< (ms) stop) (do (Thread/sleep 100) (println "."))))

(prn (System/nanoTime))

(map #(vec %) [[0 1 2] [3 4 5] [6 7 8]])
(map #(identity %) [[0 1 2] [3 4 5] [6 7 8]])
;; (map #(vector 1 2 3) [[0 1 2] [3 4 5] [6 7 8]])  ;; arity 0 => error because arity 1 expected
;; (map #([ (nth % 0) (nth % 2) ]) [[0 1 2] [3 4 5] [6 7 8]])  ;; error because % used twice
(map #(let [v %] [(nth v 0) (nth v 2) ]) [[0 1 2] [3 4 5] [6 7 8]])
(map #(subvec % 0 2) [[0 1 2] [3 4 5] [6 7 8]])
(map #(map % [0 2]) [[0 1 2] [3 4 5] [6 7 8]])  ;; using vector in function position
(map [100 200 300 400] [0 3])
(mapv [100 200 300 400] [0 3])

(def v [(let[x 1]x) (let[x 2]x) (let[x 3]) (let[])])
(filter #(int? %) v)

(def nums (map #(format "%04d" %) (range 10000)))
(def nums-filtered (for [[a b c d] nums :when (= a b) :when (= c d) :when (not= b c)] (str a b c d)))
(count nums-filtered)

;; parse and fill map
(def pattern #"([a-z]+)=([0-9]+)\s*,?\s*")
(def text "foo=123, bar=456, foobar=789")
(def varval (for [[_,var,num] (re-seq pattern text)] [(keyword var) (read-string num)]))
(def m (into {} varval))
(def vals (for [k (keys m)] (m k)))
(print (apply + vals))

(def p1 #"[0-9]+\s+")
(def p2 (re-pattern "[0-9]+\\s+"))
(re-seq p1 "123 456 789")
(re-seq p2 "123 456 789")
(def p1 #"\b[0-9]+\b")
(def p2 (re-pattern "\\b[0-9]+\\b"))
(re-seq p1 "123 456 789")
(re-seq p2 "123 456 789")

;; using matcher
(def pattern-num #"\b(?<x>\d+)\b")
(def m (re-matcher pattern-num "foo 123 bar 456 foobar 789"))
(while (.find m) (println (.group m "x")))
(println (.pattern m))

(def counter (atom 0))
;; (def pattern-num #"\b(?<varname>\w+)\s*=\s*(?<varval>\d+)\s*;\b")
(def pattern-num #"\b(?<varname>\w+)\s*=\s*(?<varval>\d+)\b\s*;")
(def m (re-matcher pattern-num "foo=123;bar =456;foobar=789;"))
(while (.find m) (println (.group m "varname") (.group m "varval")) (swap! counter inc))
(println "found " @counter " matches")

(try (/ 1 0) (throw (IllegalArgumentException.)) (println "here") (catch ArithmeticException e 1) (catch IllegalArgumentException e 2))
(try #_(/ 1 0) (throw (IllegalArgumentException.)) (println "here") (catch ArithmeticException e 1) (catch IllegalArgumentException e 2))
(try #_(/ 1 0) #_(throw (IllegalArgumentException.)) (println "here") (catch ArithmeticException e 1) (catch IllegalArgumentException e 2))
(try (throw (IllegalArgumentException. "with info")) (println "here") (catch IllegalArgumentException e (println (str e)) 0))
(try (throw (IllegalArgumentException. "with info")) (println "here") (catch IllegalArgumentException e (println (str e))))
(try (catch Exception e))

;; conversion map vector
(into {} (into [] {:a 1, :b 2}))

((juxt (fn[x]x) (fn[x](* 2 x)) (fn[x](* 3 x)) (fn[x](* x 4))) 1)
(def f (juxt (fn[x]x) (fn[x](* 2 x)) (fn[x](* 3 x)) (fn[x](* x 4))))
(def v (map f (range 10)))
(map (fn[[x _ _ _]] x) v)

(def cnt (atom 0))
(defn rand100[] (do (swap! cnt inc) (int (Math/floor (* (rand) 100)))))
(some (fn[x](= x 50)) (repeatedly rand100))
(println "cnt => " @cnt)

;; using some with maps
(some :foo [{},{}])
(some :foo [{:bar 1, :foo 10},{:foo 1}])        ;; key as function
(some {:foo 1, :bar 2} [:foobar])
(some {:foo 1, :bar 2} [:foobar, :foo, :bar])   ;; map as function

(require '[clojure.set :as s])
(some (fn[m](s/subset? #{:foo,:bar} (into #{} (keys m)))) [{:foo 1},{:bar 2},{:foo 10, :bar 20}])
(some (fn[m] (every? m [:foo, :bar])) [{:foo 1},{:bar 2},{:foo 10, :bar 20}])

;; fnil
(fnil (fn[x](println x)) 0)
((fnil (fn[x](println x)) 0) 0)
((fnil (fn[x](println x)) 0) 1)
((fnil (fn[x](println x)) 0) nil)

;; (< 10 _)
(map (partial < 10) (range 20))
(filter (partial < 10) (range 20))

;; (div2 1 _)
(defn div2[x y] (/ x y))
((partial div2 1) 10)

(def ics-raw (slurp "ics"))
(def ics-lines (clojure.string/split ics-raw #"\n"))
(def ics-set (into #{} ics-lines))
(def ics (frequencies ics-lines))
(filter (fn[[k v]] (not= v 1)) ics)

(ns-publics *ns*)
(ns-publics 'clojure.core)
(ns-publics 'clojure.string)

;; contains? is about key or index
(contains? {:foo 1} :foo)
(contains? [1] 0)
(contains? "foo" 0)
(contains? #{0} 0)

;; java contains method is about value
(.contains [1] 0)
(.contains [1] 1)
(.contains "foo" "o")

;; python has any and all
;; any([]) => False
;; all([]) => True
;; any -> some
;; all -> every?

(every? {:foo 1} [:foo])
(every? (fn[x](>= x 10)) [])
(every? (fn[x](>= x 10)) (range 9 100))
(every? (fn[x](>= x 10)) (range 10 100))
(every? #{1,2,3,4,5} [1,2,3,4,1,2,3,4])
(every? #{1,2,3,4,5} [5,6,7])

;; some returns nil, true => no ?
(some (fn[x](>= x 10)) [1,2,3,4])
(some (fn[x](>= x 10)) [1,2,3,4,10])

;; some to check if element in container
;; partial can be used even when function has one parameter
(def xs (int-array 10 (range 10)))
(def ys (into [] (range 10)))
(some (partial = 5) ys)
(some (fn[x](= x 5)) xs)
(some (partial = 10) ys)
(some (fn[x](= x 10)) xs)

;; partial
((fn[x y z](+ x y z)) 1 2 3)
((partial (fn[x y z](+ x y z))) 1 2 3)
((partial (fn[x y z](+ x y z)) 1) 2 3)
((partial (fn[x y z](+ x y z)) 1 2) 3)
((partial (fn[x y z](+ x y z)) 1 2 3))

;; count values
(frequencies (take 100 (repeatedly #(rand-int 10))))

;; combine info [[\A 1 2 3 4] [\B 5 6] [\C 7] [\D 8] [\E] [\F 0 9]]
(def xs [\A 1 2 3 4 \B 5 6 \C 7 \D 8 \E \F 0 9 \S])
;; using reduce
(def xxs (atom []))
(reduce (fn[acc x]
          (if (char? x)
            (do (reset! xxs (cons acc @xxs)) [x])   ;; acc into xxs when new char encountered, stop char needed at end
            (cons x acc)))    ;; include into acc
        []
        xs)
(pprint @xxs)

;; using doseq
(def xxs (atom []))
(def acc (atom []))
(doseq [x xs]
  (if (char? x)
    (do (reset! xxs (conj @xxs @acc)) (reset! acc [x]))
    (reset! acc (conj @acc x))))
(pprint @xxs)

(defn match-ns[fpat n] (filter (fn[f'](re-matches (re-pattern (str ".*" fpat ".*")) f')) (map str (keys (ns-publics n)))))
(match-ns "refer" 'clojure.core)
(match-ns 'refer 'clojure.core)

(defmacro Match-ns[fpat n] `(filter (fn[x#](re-matches (re-pattern (str ".*" ~fpat ".*")) x#)) (map str (keys (ns-publics ~n)))))
(Match-ns "refer" 'clojure.core)
(Match-ns 'refer 'clojure.core)

(def x1 (atom 0))
(def x2 (atom 0))
(for [i (range 10)] (swap! x1 inc))
(doseq [i (range 10)] (swap! x2 inc))
(println "x1 => " @x1 ", x2 => " @x2)
(println (= @x1 @x2))

(def x1 (atom []))
(def x2 (atom []))
(for [i (range 10)] (swap! x1 #(conj % i)))
(doseq [i (range 10)] (swap! x2 #(conj % i)))
(println "x1 => " @x1 ", x2 => " @x2)
(println (= @x1 @x2))

(def x1 (atom 0))
(def x2 (atom 0))
(for [i (range 10)] (cond true (swap! x1 inc)))
(doseq [i (range 10)] (cond true (swap! x2 inc)))
(println "x1 => " @x1 ", x2 => " @x2)
(println (= @x1 @x2))

;; str vs concat
(println (str "first," "seconds," "third," "forth"))
(println (concat "first," "seconds," "third," "forth"))

(def words ["first," "seconds," "third," "forth"])
(println (apply str words))
(println (apply concat words))
(concat [1,1.1] [2,2.1] [3,3.1] [4,4.5])

(println 1#_(2)3)
(println 1(comment 2)3)

;; using ref
(def r1 (ref 0))
(dosync (ref-set r1 (inc @r1)))
(println @r1)
;;
(def r1 (ref 0))
(dosync (alter r1 inc))
(println @r1)

(int \n)
(int \newline)

;; function call
(->> 1 identity (identity) (#(identity %)) ((fn[x]x)))
((->> 1 identity (identity) (#(identity %)) (fn[x]x)) nil)
((->> 1 identity (identity) #(identity %)) nil)

;; BigDecimal = arbitrary precision decimal point floating point numbers
(Math/pow 10.0000000000000000000001M 2)
(.pow 10.0000000000000000000001M 2)
(.pow 10.0000000000000000000001M 2M)
(.add 10M 2M)
(.subtract 10M 2M)
(.divide 10.0000000000000000000001M 2M)
(.multiply 10.0000000000000000000001M 2M)
(.negate 10M)
(.doubleValue 710.1M)
(double 710.1M)
;; (byte 128.1M)
(byte 127.1M)
(byte -127.1M)
(short 710.1M)
(int 710.1M)
(long 710.1M)
(BigDecimal. 1.1)     ;; has inaccuracy of double representation
(BigDecimal. "1.1")   ;; exact
(BigDecimal/valueOf 1.1)  ;; exact
(.toString 1.1M)
(str 1.1M)
(.compareTo 1M 1M)
(.compareTo 1.0 1.1)
(.compareTo 1 1)
;; (.compareTo 1N 1N)   ;; depends on version if method available in BigInt

(zero? 0)
(zero? 1)
(map zero? [0 1])
(map zero? [0N 1N])
(map zero? [0M 1M])

(neg? -1)
(neg? 0)
(neg? 1)
(map neg? [-1 0 1])
(map neg? [-1M 0M 1M])

(pos? -1)
(pos? 0)
(pos? 1)
(map pos? [-1 0 1])
(map pos? [-1M 0M 1M])

((juxt zero? neg? pos?) 0)
(map (juxt zero? neg? pos?) [-1M 0M 1M])
(flatten (map (juxt zero? neg? pos?) [-1M 0M 1M]))
(frequencies (flatten (map (juxt zero? neg? pos?) [-1M 0M 1M])))
((frequencies (flatten (map (juxt zero? neg? pos?) [-1M 0M 1M]))) true)
((frequencies (flatten (map (juxt zero? neg? pos?) [-1M 0M 1M]))) false)

(frequencies (map #(apply compare %) [[-1 -1],[-1 0],[-1 1],[0 -1],[0 0],[0 1],[1 -1],[1 0],[1 1]]))

(filter #(= 3 (val %)) {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5})
(get (group-by val {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5}) 3)

(map #(vector (key %) (val %)) {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5})  ;; one by one
(into '() {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5})   ;; pushing at front
(into [] {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5})
(list (into [] {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5}))   ;; wraps vector into list ([...])
(apply list (into [] {:a 1, :b 2, :c 3, :d 4, :e 3, :f 5}))   ;; outer vector --> list

;; composition
((comp (partial + 1) (partial * 10)) 1)
((comp (fn[x](inc x)) (fn[x](* 10 x))) 1)
((comp (fn[[x _]]x) (fn[x](vector x x))) 1)
((comp (fn[x](vector x x)) (fn[[x _]]x)) [1 1])

(update {:a 1, :b 2} :a inc)
(update {:a 1, :b 2} :a (fn[x y](+ x y)) 10)
(update {:a 1, :b 2} :a (fn[x y z](+ x y z)) 10 20)

; using atoms
(def a1 (atom 1))
(reset! a1 2)
(swap! a1 inc)

;; (try  (do (println "start") (throw (Exception.))) (catch Exception e (println "catched")))

;; using refs
(def r1 (ref 1))
(def r2 (ref 1))
(def r3 (ref 1))
(def r4 (ref 1))
(dosync (alter r1 inc) (alter r2 (partial + 1)) (alter r3 (fn[x](+ 1 x))) (alter r4 #(+ 1 %)))
(println @r1 @r2 @r3 @r4)
;; with exception
(def r1 (ref 1))
(def r2 (ref 1))
(def r3 (ref 1))
(def r4 (ref 1))
(try (dosync (alter r1 inc) (alter r2 (partial + 1)) (alter r3 (fn[x](+ 1 x))) (alter r4 #(+ 1 %)) (throw (Exception.))) (catch Exception e (println "catched")))
(println @r1 @r2 @r3 @r4)
;;
;; dosync has no effect on atoms, no roll-back with exception
(def r1 (atom 1))
(def r2 (atom 1))
(def r3 (atom 1))
(def r4 (atom 1))
(try (dosync (alter r1 inc) (alter r2 (partial + 1)) (alter r3 (fn[x](+ 1 x))) (alter r4 #(+ 1 %)) (throw (Exception.))) (catch Exception e (println "catched")))
(println @r1 @r2 @r3 @r4)

;; s already aliased to set => error when using s
(require '[clojure.string :as s1 :refer [split] :rename {reverse rev}])
;; no rev here
(require '[clojure.string :as s1 :refer [split, reverse] :rename {reverse rev}])
;; now rev available

(clojure.core/replace {"a" "b"} ["a" "b" "c"])
(clojure.core/replace {1 2} [1 2 3])

(assoc {} :a 1)
(assoc (assoc {} :a 1) :a 2)
(assoc {} :a 1 :b 2)
(dissoc (assoc {} :a 1 :b 2) :a :b)
(dissoc (assoc {} :a 1 :b 2) :a :b :c)

(dissoc {:a 1, :b 2, :c 3, :d 4})
(dissoc {:a 1, :b 2, :c 3, :d 4} :a)
(dissoc {:a 1, :b 2, :c 3, :d 4} :a :b)
(dissoc {:a 1, :b 2, :c 3, :d 4} :a :b :c)
(dissoc {:a 1, :b 2, :c 3, :d 4} :a :b :c :d)
;;
(update {:a 1} :a (fn[_]2))
(update {:a 1} :a (fn[_ _]2) :unused)
(update {:a 1} :a inc)
;; the deepest key can be created
(update {} :a (fn[_]1))
(update {} :a (fn[_ x]x) 1)
;; (update-in {{}} [:a :b] (fn[_]1))
(update-in {:a {}} [:a :b] (fn[_]1))

;; transient, persistent
(def tv (transient []))
(conj! tv 1)
(conj! tv 2)
(conj! tv 3)
(conj! tv 4)
(count tv)
(def pv (persistent! tv))
;; after persistent no changed possible
;; (conj! tv 5)

;; sorted map
(sorted-map)
(sorted-map :a 1)
(type (sorted-map))   ;; persistent tree map
(type {})             ;; persistent array map
(sort (sorted-map :d 4 :b 2 :c 3 :a 1))
(sort {:d 4 :b 2 :c 3 :a 1})

;; sorted set
(sorted-set)
(sorted-set 1 2 3 4)
(type (sorted-set))   ;; persistent tree set
(type #{})            ;; persistent hash set
(sort (sorted-set 4 2 3 1))
(sort #{4 2 3 1})

(for[i #{1 2 3 4}] i)
(for[i (hash-set 1 2 3 4)] i)
(for[i (sorted-set 1 2 3 4)] i)

;; mixed types
(for[kv (hash-map 1 2 3 4 "a" "A")]kv)
(for[[k v] (hash-map 1 2 3 4 "a" "A")]k)
;; no mixed types
(for[kv (sorted-map 1 2 3 4)]kv)
(for[kv (sorted-map "a" "A" "b" "B")]kv)

;; java.io
(require '[clojure.java.io :as io])
(def data (io/file "data.txt"))

(set! *warn-on-reflection* true)
(.toString (long 1))
(.toString (Long. 1))

(def a (atom []))
(dotimes [_ 10] (swap! a #(conj % (rand))))
(print @a)

(def a (atom {}))
(def a (atom (sorted-map)))
(dotimes [k 10] (swap! a #(assoc % k (rand))))
(print@a)

(type *out*)
(type *in*)
(.write *out* 49)
(.write *out* "1")

;; using unter WSL
;; cd /mnt/c && mkdir clojure
;; cd clojure && touch tools.clj
;; edit tools.clj
;; start clojure with
;; clojure -cp "/mnt/c/clojure"
;; (require 'tools)
;; (tools/hexify 100)
;; (require '[tools :as t :refer [hex] :rename {hex hexify}])
;; (require 'tools :reload)

;; usually the bindings remain after the require
;; changed functions will not be updated => add :reload
(require 'clojure.string :reload)

(def public-keyword-a :a)
(def local-keyword-a ::a)

;; (partition 3 0 (range 10))
(take 10 (partition 3 0 (range 10)))
(map (juxt inc dec) [1 2 3 4])
(map (juxt (fn[[x y]](+ x y)) (fn[[x y]](* x y))) (partition 2 1 [1 2 3 4]))

(defstruct foo :a :b)
(def f1 (struct foo 1 2))
(:a f1)

(defrecord bar [a b])
(def b1 (bar. 1 2))
(:a b1)

;; defonce
;; (def show nil)
(defmulti show identity)
(defmethod show 1 [x] "one")
(defmethod show 2 [x] (println "two"))
(defmethod show :default [x] (printf "something else %s\n" x))
(show 1)
(show 2)
(show 3)

(defmulti show (fn[x] (if (and (> x 0) (< x 100)) x 0)) :default 0)
(defmethod show 1 [x] "one")
(defmethod show 2 [x] (println "two"))
(defmethod show 3 [x] 3)
(defmethod show 0 [x] (printf "outside the range or not implemented %s\n" x))
(show 1)
(show 2)
(show 3)
(show 101)

(defmulti show :tag)
(defmethod show 1 [m] (:name m))
(defmethod show :default [m] m)
(show {:tag 1, :name "foo"})
(show {:tag 2})
(show {})

(try (catch Exception e) (finally))
(try (catch IndexOutOfBoundsException e) (catch Exception e) (finally))
(try (catch ArithmeticException e) (catch IndexOutOfBoundsException e) (catch Exception e) (finally))
(try (catch ArithmeticException e) (catch IndexOutOfBoundsException e) (catch Exception e) (finally (printf "done\n")))
(try (/ 1 0) (catch ArithmeticException e 1) (catch IndexOutOfBoundsException e 2) (catch Exception e 3) (finally (printf "done\n")))
(try (nth [] 0) (/ 1 0) (catch ArithmeticException e 1) (catch IndexOutOfBoundsException e 2) (catch Exception e 3) (finally (printf "done\n")))
(try (+ 1 "2") (nth [] 0) (/ 1 0) (catch ArithmeticException e 1) (catch IndexOutOfBoundsException e 2) (catch ClassCastException e 3) (catch Exception e 0) (finally (printf "done\n")))

(try (throw (Exception. "here")) (slurp "no_file") #_(NoSuchClass.) ((fn[_])) (+ 1 "2") (nth [] 0) (/ 1 0)
     (catch ArithmeticException e 1)
     (catch IndexOutOfBoundsException e 2)
     (catch ClassCastException e 3)
     (catch clojure.lang.ArityException e 4)
     (catch IllegalArgumentException e 5)
     (catch java.io.FileNotFoundException e 6)
     (catch Exception e 0)
     (finally (printf "done\n")))

(try (/ 1 0) (catch Exception e))
(try (/ 1 0) (catch Exception e nil))
(try (/ 1 0) (catch Exception e 0))
(try (/ 1 0) (catch Exception e "divided by zero"))

(def f (constantly 1))
(apply f nil)
(apply f '())
(apply f [])

;; simple alternative to cond and condp
(case 0, 1 [], 2 '(), 3 {}, 4 #{}, 0 nil)
(defn f[v] (case v, 1 [], 2 '(), 3 {}, 4 #{}, 0 nil))

(str/join ["a" "b" "c" "d"])
(apply str/join [["a" "b" "c" "d"]])

(str/join "," ["a" "b" "c" "d"])
(apply str/join ["," ["a" "b" "c" "d"]])

(take-nth 2 (range 100))

(empty [1, 2, 3, 4, 5, 6])
(empty (list 1, 2, 3, 4, 5, 6))
(empty {:one 1, :two 2})
(empty #{1,2})

(def xs [:one, :two, :three, :four])
(def ys [1, 2, 3, 4])
(partition 2 (interleave xs ys))
(map (fn[x y] [x y]) xs ys)

(println "0 | 1 => ")
(case (read) 0 "null" 1 "eins")
(println "0 | 1 => ")
(take 2 (repeat (case (read) 0 "null" 1 "eins")))
(println "0 | 1 => ")
(for [_ (range 2)] (case (read) 0 "null" 1 "eins"))

(if '() 1 2)
(if [] 1 2)
(if {} 1 2)
(if #{} 1 2)
(if "" 1 2)
(if 0 1 2)
(defn f[xs] (if-let [it xs] [(reduce + it) (reduce * it)]))

;; when
(when true)
(when-not false)

(when true (println "1") (println "2"))
(if true (do (println "1") (println "2")))

(require '[clojure.walk :as w])
(w/walk #(nth % 0) #(apply max %) [[0, 1, 2, 3] [4, 5, 6, 7] [8, 9, 10] [11]])
(w/walk count #(apply max %) [[0, 1, 2, 3] [4, 5, 6, 7] [8, 9, 10] [11]])     ;; (count) requires IFn Counted => clojure.lang.Counted

;; prewalk iterates over outer elements first => 1, [2.0, 2.1], 2.0, 2.1 ...
;; postwalk iterates over outer elements last => 1, 2.0, 2.1, [2.0, 2.1] ...
(def nested_list [1, [2.0, 2.1], 3, 4])
(w/prewalk (fn[x] (println x) (if true x 0)) nested_list)
(w/postwalk (fn[x] (println x) (if true x 0)) nested_list)
(count (flatten nested_list))

;; Illegal state exception -> cannot dynamically bind non-dynamic var
;; (def x 1)
;; (binding [x 2])

(def ^{:dynamic true} x 1)
(println "before x => " x)
(binding [x 2] (println "inside x => " x))
(println "after x => " x)

(def ^{:private true} x "doc string" 1)
(def ^:private x "doc string" 1)

(defn ^{} f "docstring" ([x] 1) ([x & xs] 2))

;; side-effect, then return value
(defn p[x] (printf "%s\n" x) x)
(def lz (for[i (range 10)] (p i)))
(take 1 lz)   ;; side-effects of all 10, returned only the first
(take 1 lz)   ;; no side-effects, returned only the first
;; lazy sequence evaluted in blocks
(def lz (for[i (range 100)] (p i)))
(take 1 lz)   ;; side-effects of 32 (0..31), returned only the first
(take 1 lz)   ;; no side-effects, returned only the first

;; python
;; round(1/3, 5)
;; round(0.12345_499, 5)
;; round(0.12345_500, 5)
;; round(0.12345_501, 5)
;; with-precision only used with BigDecimal
(with-precision 5 (/ 1 3M))
(with-precision 5 (/ 4 3M))
(with-precision 5 :rounding FLOOR (/ 1 3M))
(with-precision 5 :rounding CEILING (/ 1 3M))
(with-precision 5 :rounding HALF-UP (/ 1 3M))
(with-precision 5 :rounding HALF-DOWN (/ 1 3M))
(with-precision 5 :rounding HALF-EVEN (/ 1 3M))
(with-precision 5 :rounding UP (/ 1 3M))
(with-precision 5 :rounding DOWN (/ 1 3M))
(with-precision 5 :rounding UNNECESSARY 0.1234567)

;; maybe bug, v3 and v4 work
(defn with-prec-v1[n x] (double (with-precision n (* 1M x))))
(with-prec-v1 4 0.12345)
(defn with-prec-v2[n x] (double (with-precision n (BigDecimal. x))))
(with-prec-v2 4 0.12345)
(defn with-prec-v3[n x] (double (with-precision n (* 1 (BigDecimal. x)))))
(with-prec-v3 4 0.12345)
(defn with-prec-v4[n x] (double (with-precision n (* 1M (BigDecimal. x)))))
(with-prec-v4 4 0.12345)

(with-precision 4 1.23456)    ;; 1.23456
(with-precision 4 1.23456M)   ;; 1.23456M
(with-precision 4 (BigDecimal. 1.23456))  ;; 1.2345600000000001017497197608463466167449951171875M
(with-precision 4 (* 1 (BigDecimal. 1.23456)))  ;; 1.235M
(with-precision 4 (+ 0 (BigDecimal. 1.23456)))  ;; 1.235M

;; in namespace user
(ns other)
(defn f "doc of f" [n] (* 2 n))
;; (doc f)    ;; will not work
(require 'clojure.repl)
(clojure.repl/doc f)
;; (dir other)    ;; will not work
(clojure.repl/dir other)

(defn f "without cache, with side effect" [n] (let [r (* 2 n)] (println "f called with " n " returns " r) r))
(def f' "cached version of f, side effect takes place once" (memoize f))
(f' 100)
(f' 100)

(defn sup [n] (let [s (atom 0)] (dotimes [i n] (swap! s (partial + i))) @s))
(sup 10)
(apply + (range 10))
(def N 1000000)
(time (sup N))
(time (apply + (range N)))

(require '[clojure.java.io :as io])
(.exists (io/as-file ""))
(.exists (io/as-file "."))
(.exists (io/as-file ".."))
(.exists (io/as-file "..."))

;; eval
(def s "(+ 1 2)")
(eval s)  ;; does not work
(eval (read-string s))  ;; works
;;
(def s '(+ 1 2))
(eval s)  ;; works
;; echo "1 2 3 4" | clojure -e '(eval (read-string (format "(apply + [%s])" (read-line))))'

;; reading from command line
(read)
(read-line)
(read-string (read-line))   ;; same as (read)
(slurp *in*)

(Long. "0")
(Long/parseLong "0")
(Long/parseLong "1111" 2)
(Long/decode "1111" 2)
(Long/decode "0x10")
(Long/toBinaryString 15)

;; how to split any sequence
(partition 3 (range 10))
(partition-all 3 (range 10))
(defn p[xs n] (map #(count %) (partition n xs)))
(defn p-all[xs n] (map #(count %) (partition-all n xs)))
;; str sequence = list of chars
(seq "foobar")
(apply str (seq "foobar"))
(partition 3 "foobar")
(map #(apply str %) (partition 3 "foobar"))

;; keep-indexed takes f and sequence
;; (f index value)
(defn findall[xs x] (keep-indexed #(if (= %2 x) %1) "abcdefooebarefoobar"))
(defn findall'[xs x] (keep-indexed #(if (= %2 x) %1 nil) "abcdefooebarefoobar"))   ;; explicit else nil
(defn findall''[xs x] (keep-indexed #(if (= %2 x) %1 false) "abcdefooebarefoobar"))   ;; false is not nil => does not work
(println "findall => " (findall "abcdefooebarefoobar" \e))

;; python version
;; def findall(xs,x): return [i for i,x_ in enumerate(xs) if x_==x]

;; keep, filter
(keep #(if (= % 1) %) [1 2 3 4 5 1 2 3 4 5])
(filter #(= % 1) [1 2 3 4 5 1 2 3 4 5])
;; keep can do more than filter
(def c (atom 0))
(keep #(if (= % 1) (swap! c inc)) [1 2 3 4 5 1 2 3 4 5])
(def c (atom 0))
(keep #(if (= % 1) (swap! c (fn[v] (+ @c %)))) [1 2 3 4 5 1 2 3 4 5])
(keep #(if (> % 5) [% (. System currentTimeMillis)]) [1 2 3 4 5 1 2 3 4 5 6 7])

(dir user)
(dir clojure.core)
(dir clojure.repl)
(dir clojure.string)
(dir clojure.pprint)

(map char "foo")
(map int "foo")
(map long "foo")

(def ts #inst "2023")
(def id #uuid "3b8a31ed-fd89-4f1b-a00f-42e3d60cf5ce")

(type 1)    ;; returns the :type metadata of var or its class
(class 1)   ;; returns the class of var
(clojure.core/type 1)    ;; returns the :type metadata of var or its class
(clojure.core/class 1)   ;; returns the class of var

(def ^{:type 'enum} x 1)
(meta (var x))

(def x (with-meta [1] {:type :foo-type}))
(meta x)
(type x)
(class x)

*data-readers*
(meta (var *data-readers*))

(def v)     ;; unbound
(meta #'v)
(def v 1)   ;; bound to 1
(meta #'v)
(def v)     ;; remains bound to 1

(def t (Thread. (fn[](println 1))))
(.start t)

(def r (atom (Random. 0)))
(defn random-1000[] (mod (Math/abs (.nextInt @r)) 5000))

(def threads (atom []))
(dotimes [i 10] (swap! threads #(conj % (Thread. (fn[] (do (Thread/sleep (random-1000)) (println i)))))))
(doseq [t @threads] (.start t))
(doseq [t @threads] (.join t))

(partition-by (fn[_]false) (range 100))
(partition-by (fn[_]true) (range 100))
(partition-by (fn[n](<= n 50)) (range 99))
(group-by (fn[n](<= n 50)) (range 99))  ;; more
;; [1 2 3 4 [10 20 30 40 [100 200 300 400] 50 60 70 80] 5 6 7 8] => [4 [ 4 [ 4] 4 ] 4]
(apply count [[1 2 3 4]])
(count [1 2 3 4])

(import java.util.Random)
(def r (Random.))
(.nextInt r)
(require 'clojure.reflect)
(clojure.reflect/reflect r)
(keys (clojure.reflect/reflect r))  ;; [:bases, :flags, :members]
(:bases (clojure.reflect/reflect r))    ;; hash-set
(:flags (clojure.reflect/reflect r))    ;; hash-set
(:members (clojure.reflect/reflect r))  ;; hash-set
(for [m (:members (clojure.reflect/reflect r))] (println m))
(require 'clojure.pprint)
(for [m (:members (clojure.reflect/reflect r))] (clojure.pprint/pprint m))
(clojure.pprint/pprint (:members (clojure.reflect/reflect r)))
;;
(def m (:members (clojure.reflect/reflect r)))
(map :name m)
(map :name (filter #(= #{:public} (:flags %)) m))

(.limit (.doubles r 10) 4)
(.forEach (.limit (.doubles r 10) 4) #(print %))

;; (defmulti ptype type)
(defmulti ptype type :default nil)
;; (defmulti ptype type :default 0)
(defmethod ptype Long [x] (inc x))
(defmethod ptype Character [x] (int x))
(defmethod ptype String [x] (clojure.string/upper-case x))
(defmethod ptype clojure.lang.PersistentList [x] (count x))
(defmethod ptype clojure.lang.PersistentList$EmptyList [x] 0)
(defmethod ptype clojure.lang.PersistentVector [x] (count x))
(defmethod ptype clojure.lang.PersistentArrayMap [x] (keys x))
(defmethod ptype nil [_] nil)
;; (defmethod ptype nil [_] "using default nil")
;; (defmethod ptype :default [_] "using default")
;; (defmethod ptype 0 [_] "using default 0")
(ptype 1)
(ptype "foo")
(ptype '())
(ptype '(1 2 3 4))
(ptype [1 2 3 4])
(ptype {:a 1, :b 2, :c 2, :d 4})

(vector 1 2 3 4)
(vec (range 1 5))
(vector)
(vec nil)
(vec ())
(vec '())
(vec [])
(into (vector) '())
(into (vec nil) '())

(defrecord E)
(defrecord W [a b c d])
(def w1 (W. 1 2 3 4))
(def w1' (with-meta w1 {:tag "record"}))
(meta w1')
(def ^{:tag "record"} w2 (W. 1 2 3 4))
(meta w2)
(meta (var w2))

(Class/forName "java.lang.Byte")
(Class/forName "clojure.lang.PersistentHashMap")
(Class/forName "[Ljava.lang.Byte;")
(def B (Class/forName "java.lang.Byte"))
(def BA (Class/forName "[Ljava.lang.Byte;"))
(def M (Class/forName "clojure.lang.PersistentHashMap"))
[B BA M]

(defprotocol P (self[x]) (inc-with-limit[x n]))
(extend-protocol P java.lang.Long (self[x]x) (inc-with-limit[x n] (min (inc x) n)))
(self 10)
(inc-with-limit 9 10)
(inc-with-limit 10 10)
;; (self (int 0))
;; (self (Integer. "0"))
(= (byte 0) (long 0))
(= (byte 0) 0N)
(= (byte 0) 0M)
(= (byte 0) (double 0))

(cond false 1, false 2, false 3, false 4, true :other)
(condp (fn[x y] (println "x=" x " y=" y) (= x y)) :param :x 0, 1 2, 3 4, 5 6, 7 8 :other)

(if (#{1 2 3 4} 0) :if :else)
(get #{1 2 3 4} 0)
(get #{1 2 3 4} 0 :not-found)

(let [x0 0, [x1] (list 1), [x2 x3] (list 2 3)] x0)
(let [x0 0, [x1] (list 1), [x2 x3] (list 2 3)] x1)
(let [x0 0, [x1] (list 1), [x2 x3] (list 2 3)] x2)
(let [x0 0, [x1] (list 1), [x2 x3] (list 2 3)] x3)

;; special symbols
(* ##NaN 0)
(+ ##Inf 1)
(/ ##Inf ##-Inf)
;; (defmacro current-namespace[] *ns*)
(defmacro current-namespace[] (str *ns*))
(def cur-namespace 'user)
(ns a)
(print *ns*)
(def a-k ::keyword)
(ns b)
(print *ns*)
(def b-k ::keyword)
;; (ns user)
;; (ns current-namespace)
;; (ns (user/current-namespace))
;; (ns user/cur-namespace)
;; (in-ns 'user)
(in-ns user/cur-namespace)
(print *ns*)

clojure.core/*ns*

(map (fn[f](f 1 1)) [+ - * /])

(import java.lang.Thread)
(def t1 (Thread. (fn[](println "a")(Thread/sleep 1000)(println "b"))))
(.start t1)
(def f1 (future (println "\na")
                (Thread/sleep 10000)
                (println "b\n")
                1))
(println @f1)

(-> 1)
(->> 1)
(some-> 1)
(some->> 1)
(as-> [1 2 3 4] xs (map (fn[x](* 2 x)) xs))

;; complementary to map, map same function for different values in container
;; cond->, cond->>, different functions for same container
(defn describe-it[n]
  (cond-> []
    (> n 100) (conj ">100")
    (= Double (type n)) (conj "double")
    (= Float (type n)) (conj "float")))

;;         v---------------------------------------------
;;           v---------------------v  |        |        |
(condp (fn[x y](println x)(= x y)) 10 0 "null" 1 "eins" 10 "zehn")

(cond-> [] true (conj 1)
           false (conj -1)
           true (conj 2)
           false (conj 0)
           true (conj 3)
           false (conj 1)
           true (conj 4))

(def ct (Thread/currentThread))
(def t (Thread. (fn[](prn "a")(Thread/sleep 1000)(prn "b"))))
(def t0 (System/currentTimeMillis))
(.start t)
(.join t)
(def t1 (System/currentTimeMillis))
(prn "t1-t0=" (- t1 t0) "ms")
;; (.join ct)

;; thread needs to be created and started
;; thread needs to be joined when creator thread wants to block until thread is finished
;; thread can return results via atom
(def r (atom {}))
(def t (Thread. (fn[] (reset! r {}) (Thread/sleep 10000) (swap! r #(conj %1 [:result (rand)])))))
(.start t)
(.join t)   ;; blocked here waiting for thread to finish
(println @r)

;; future runs when created
;; future can return results directly
(def f (future (Thread/sleep 10000) {:result (rand)}))
@f  ;; blocked here waiting for future (pool thread) to finish

(zipmap [\a \b \c \d] [1 2 3 4])
(zipmap "foobar" (range 100))
(def random-nums (repeatedly rand))
(take 10 random-nums)

(defn f[x]
(let [y (or x 0)] y))
(f -1)
(f 0)
(f nil)
(f 1)

(dorun (for[i (range 10)] i))   ;; side-effect
(doall (for[i (range 10)] i))   ;; side-effects and keep values

(dorun (for[i (range 10)] (println i)))   ;; side-effect, value nil
(doall (for[i (range 10)] (println i)))   ;; side-effect, value nil

(dorun (for[i (range 10)] (do (println i) 1)))  ;; side-effect, value 1
(doall (for[i (range 10)] (do (println i) 1)))  ;; side-effect, value 1

(prn (for [i "abcd", j (range 4)] `(~i ~j)))
(prn (into [] (for [i "abcd", j (range 4)] `(~i ~j))))
(prn (for [i "abcd", j (range 4)] (vector i  j)))
(prn (into [] (for [i "abcd", j (range 4)] (vector i  j))))

;; :exit, :out, :err
(require '[clojure.java.shell :as shell])
;; (shell/sh "/bin/bash" "-c" "echo test")
(print (:out (shell/sh "bash" "-c" "ls")))
(print (:out (shell/sh "bash" "-c" "pwd")))

;; destructuring works with macros
(defmacro e2[[arg1 op arg2]] `(~op ~arg1 ~arg2))

(defmacro swap[[op arg1 arg2]] `(~op ~arg2 ~arg1))
(println '(swap (- 5 10)) "="   (swap (- 5 10)))

(defmacro e [f]
  (cond (not (seq? f)) f                    ;; expr -> expr
        (= 1 (count f)) `(e ~(first f))     ;; (expr) -> expr
        true (let [op (second f),
                   arg1 (first f),
                   arg2 (rest (rest f))]
               `(~op (e ~arg1) (e ~arg2)))))

(prn (e (1 + 1)))
(prn (e (2 * 2)))
(prn (e (2 * (1 + 1))))
(prn (e (2 * (1 + (11 - 10)))))

;; (first nil) => nil
;; (last nil) => nil
;; (next nil) => nil
;; (rest nil) => ()
(let [xs [1 2 3 4]] 
  [(next xs)
   (rest xs)
   (first xs)
   (last xs)
   ])

;; rest returns () when provided with nil or empty sequence
;; other return nil when provoded with nil or empty sequence
(map (fn[f](f nil)) [first last next rest])
(map (fn[f](f [])) [first last next rest])
(map (fn[f](f [1])) [first last next rest])
(map (fn[f](f [1 2])) [first last next rest])
(map (fn[f](f [1 2 3])) [first last next rest])

(def m {:a 1, :b 2, :c 3, :d 4})
(def m' (into {} (map (fn[[k v]] [v k]) (into [] m))))
(defn map-invert[m] (into {} (map (fn[[k v]] [v k]) (into [] m))))
(defn map-invert-maybe[m] 
  (let [m-size (count m)
       m' (into {} (map (fn[[k v]] [v k]) (into [] m)))
       m'-size (count m')]
      (if (= m'-size m-size) [true m'] [false m'])))

;; in clojure only "false" values are => nil, false
(and 0 [] {} #{} '() "")  ;; "true", last "true" value returned => ""
(and 0 [] {} nil #{} '() "")  ;; "false", first "false" value returned => nil
(and 0 [] false {} nil #{} '() "")  ;; "false", first "false" value returned => false
(or nil false 1 "")   ;; "true", first "true" value returned => 1
(or nil false "" 1)   ;; "true", first "true" value returned => ""

;; returns arrays
(.getFields System)
(.getMethods System)
(.getConstructors System)

(require '[clojure.reflect :as r])
(resolve 'r/reflect)
(def rs (r/reflect System))
(keys rs)
(:bases rs)
(:flags rs)
(:members rs)   ;; fields, methods, constructors
(map type (:members rs))
(map :name (:members rs))

(def mrs (:members rs))
(for[m mrs] (printf "%s : %s -> %s ... %s\n" (:name m) (:parameter-types m) (:return-type m) (:flags m)))

(defn is-field[m] (printf "%s : %s ... %s\n" (:name m) (:type m) (:flags m)))
(defn is-method[m] (printf "%s : %s -> %s ... %s\n" (:name m) (:parameter-types m) (:return-type m) (:flags m)))
(defn is-constructor[m] (printf "%s : %s -> void ... %s\n" (:name m) (:parameter-types m) (:flags m)))
(for [m mrs]
  (do (condp = (count (keys m))
          4 (is-field m)
          5 (is-constructor m)
          6 (is-method m)
          nil)))

(System/setIn nil)
(System/setOut nil)
(System/setErr nil)
(System/in)
(System/out)
(System/err)

(require '[clojure.pprint :as pp])
(pp/pprint (:members (r/reflect System)))
(pp/pprint (:members (r/reflect java.io.File)))
(pp/pprint (:members (r/reflect java.util.Set)))
(pp/pprint (:members (r/reflect java.util.Date)))
(pp/pprint (:members (r/reflect java.util.Currency)))
(pp/pprint (:members (r/reflect java.awt.Paint)))
(pp/pprint (:members (r/reflect java.awt.Rectangle)))
(pp/pprint (:members (r/reflect java.math.BigDecimal)))
(pp/pprint (:members (r/reflect java.math.BigInteger)))
(pp/pprint (:members (r/reflect java.math.MathContext)))
(pp/pprint (:members (r/reflect java.time.LocalDate)))
(pp/pprint (:members (r/reflect java.lang.Cloneable)))
(pp/pprint (:members (r/reflect java.lang.Compiler)))
(pp/pprint (:members (r/reflect java.lang.System)))
(pp/pprint (:members (r/reflect java.lang.Thread)))

(pp/pprint (:members (r/reflect java.util.regex.Matcher)))
(pp/pprint (:members (r/reflect java.util.regex.Pattern)))

(def m (java.util.regex.Pattern/compile "foo"))
(.matcher m "foo")
(println (java.util.regex.Pattern/DOTALL))

;; WSL1 (installed)
;; export DISPLAY=:0

;; WSL2
;; export DISPLAY=:0.0

;; Can't load library: /usr/lib/jvm/java-16-openjdk-amd64/lib/libawt_xawt.so
;; (java.awt.Frame.)
;; (java.awt.Dialog.)

;; static function
(java.util.Currency/getAvailableCurrencies)

(condp (fn[i x](= i x)) (read) 1 "eins")
(condp (fn[i x](= i x)) (read) 1 "eins" nil)

;; how to use walk
;; simple example
(clojure.walk/walk identity identity [1 2 3 4])
(clojure.walk/walk inc identity [1 2 3 4])
(clojure.walk/walk identity #(apply + %) [1 2 3 4])
(clojure.walk/walk inc #(apply + %) [1 2 3 4])

;; complex example
(def c [[1] [1 2] [1 2 3] [1 2 3 4]])
(clojure.walk/walk first identity c)  ;; [1 1 1 1]
(clojure.walk/walk last identity c)   ;; [1 2 3 4]
(clojure.walk/walk count identity c)  ;; [1 2 3 4]
(apply + (flatten c))                             ;; 20
(clojure.walk/walk #(apply + %) #(apply + %) c)   ;; 20

(def c1 [[1] [1 2] [1 2 3] [1 2 3 4] [1 2 3 4 5 [6]]])
(clojure.walk/postwalk (fn[x](if (coll? x) (apply + x) x)) c1)
(apply + (flatten c1))

(def c1 [[1] [1 2] [1 2 3] [1 2 3 4] [1 2 3 4 5 [6]]])
(def c1 ["txt"])
(def c1 "txt")
(def c1 [0 [1] [1 2] [1 2 3] [1 2 3 4] [1 2 3 4 5 [6]]])
(clojure.walk/prewalk (fn[x](if (coll? x) (count x) x)) c1)

;; complex example
(def d [{:id 0, :n 100, :c 0} {:id 1, :n 200, :c 1} {:id 2, :n 300, :c 1} {:id 3, :n 100, :c 1}])
(clojure.walk/walk :id identity d)
(clojure.walk/walk :n (fn[x](apply + x)) d)
(clojure.walk/walk #(update % :c inc) identity d)
(clojure.walk/walk #(update % :c inc) (fn[x](apply + (map :c x))) d)

(def m {:x0 true, :x1 false, :x2 true, :x3 false})
(update m :x0 (fn[x](if x 1 0)))
(update m :x0 (fn[x y](if x y 0)) 10)

(def nested-m {:p {:p nil, :v 0}, :v 1})
(update-in nested-m [:p :v] inc)

;; transduce is function which applies transducer to collection and reduces it to one value
(transduce cat conj [] [[1] [2] [3] [4]])
(transduce cat conj [] [[1] [2] [3] [4 [5]]])
(transduce (comp cat) conj [] [[1] [2] [3] [4 [5]]])
(transduce (comp cat (filter even?)) conj [] [[1] [2] [3] [4]])

((comp inc inc) 0)

;; + is object
((-> +) 0)
((-> +) 0 1)
((->> +) 0)
((->> +) 0 1)

(-> 0 +)
(->> 0 +)
(-> 0 ((fn[x](+ x))))
(->> 0 ((fn[x](+ x))))

(->> [1 2 3 4] (take 2))
(-> [1 2 3 4] (count))

(transduce (take 5) + (range))
(transduce (take 5) + 10 (range))

(apply str (mapcat #(take 2 %) ["foo" "bar" "baz"]))
(apply str (flatten (map #(take 2 %) ["foo" "bar" "baz"])))

(type (comp cat (filter even?)))
(type (transduce cat conj []))
(type (sequence cat []))
(type (eduction cat []))

(defn f[x y] (+ x y))
((fnil f 0) 0 0)
((fnil f 0) nil 0)
((fnil f 0 0) 0 nil)
((fnil f 0 0) nil nil)

(defn q''[& r] (count r))

(defn q ([]0)
        ([x]1)
        ([x & r](+ 1 (count r))))

(def q' (fn ([]0)
            ([x]1)
            ([x & r](+ 1 (count r)))))

;; custom transducer
(transduce 
  (filter even?)  ;; transducer
  (fn                 ;; reducer
    ([x]x)            ;; arity 1
    ([x y](+ x y)))   ;; arity 2
    0                 ;; initial value
    (range 100)
  )

;; using +
(transduce 
  (filter even?)  ;; transducer
    +             ;; (doc +) => [] [x] [x y] [x y & r]
    0
    (range 100)
  )

;; different styles
(reduce + (filter odd? (map #(+ 2 %) (range 0 10))))

(def xform 
  (comp
    (partial filter odd?)
    (partial map #(+ 2 %))))
(reduce + (xform (range 0 10)))

(defn xform[xs]
  (->> xs
       (map #(+ 2 %))
       (filter odd?)))
(reduce + (xform (range 0 10)))

(+)           ;; 0 [] [x] [x y] [x y & r]
(- 0)         ;; 0 [x] [x y] [x y & r]
(*)           ;; 1 [] [x] [x y] [x y & r]
(/ 1)         ;; 1 [x] [x y] [x y & r]
(> 10)        ;; true
(< 10)        ;; true
(> 10 5 1)    ;; true
(< 1 5 10)    ;; true
(str)         ;; ""
(list)        ;; () persistent list
(vector)      ;; [] persistent vector
(hash-map)    ;; {} persistent array map ... this is optimization
(hash-map :a 1)    ;; {} persistent hash map 
(array-map)   ;; {} persistent array map
(sorted-map)  ;; {} persistent tree map
(hash-set)    ;; #{}  persistent hash set
(sorted-set)  ;; #{}  persistent tree set

(type (hash-map))
(type (hash-map :a 1))
(type (array-map))
(type (array-map :a 1))

(def ss (sorted-set))
(defn random-1-100[](int (* 100 (rand))))
(dotimes [_ 10] (def ss (conj ss (random-1-100))))
(doseq [] (for[i ss] (+ 100 i)))

(def sm (sorted-map))
(def lowercase-letters "abcdefghijklmnopqrstuvwxyz")
(def k (atom #{}))
(dotimes [_ 10] (swap! k conj (take 1 (shuffle (seq lowercase-letters)))))
(def k2 (atom #{}))
(dotimes [_ 10] (swap! k2 conj (rand-nth (seq lowercase-letters))))

;; partial only binds from left to right

(keyword "x") ;; from str to keyword
(symbol "x")  ;; from str to symbol
(name 'x)     ;; from symbol to str

(->> (range)
     (take 10)
     (shuffle))

(->> (range)
     (take 10)
     (shuffle)
     (filter (partial > 1))
     (filter (partial < 7))
     )


