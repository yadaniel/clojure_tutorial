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

(Integer/parseInt "10")
(Integer/parseInt "10" 10)
(Integer/parseInt "10" 2)
(Integer/parseInt "10" 16)

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
(def l (make-array Long 10))
(pprint l)
(aset l 0 1)
(pprint l)
(println (count l))

;; repl
(require '[clojure.pprint :as pp :refer [pprint]])
(clojure.pprint/pprint 1)
(pp/pprint 1)
(pprint 1)
(resolve 'pprint)
(clojure.main/repl :print :pprint)

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


