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

;; import is function
;; :import is keyword in context of ns
;; either use 
;; (ns new-namespace)
;; (import 'something)
;; or
;; (ns new-namespace :import 'something)

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

;; both of type ratio
(def f1 1/7)
(def f2 (/ 1 7))

(defn files[dir] (file-seq (clojure.java.io/file dir)))
(count (files "."))

(use 'clojure.java.io)
(-> "./" file .listFiles)
(.listFiles (file "./"))

(take 10 (range 100))
(drop 10 (range 100))
(reverse (range 10))

(take-nth 2 (range 100))  ;; every second element
(take-while #(< % 10) (range 100))
(drop-while #(< % 10) (range 100))

(slurp "data.txt")
(slurp "data.txt" :encoding "ascii")
(slurp "data.txt" :encoding "utf8")
;; (slurp "data.txt" :encoding "utf9")   ;; exception
(def txt (slurp "data.txt"))


;; :import brings in java classes
;; :require brings in namespaces, access with qualification
;; :use brings in namespaces, access without qualification
;; :requre, :refer => :use
;;
;; (:require [foo :as f])
;; (:require [foo :refer [bar baz]])

(println (.getTime (Date.)))
(.print System/out "enter: ")
(.readLine (System/console))
;; other methods
(.read System/in)
(.print System/out "stdout")
(.print System/err "stderr")
(.println System/out "stdout")
(.println System/err "stderr")

;; using regex
(defn is_number?[n]
  (let [p (re-pattern "\\d+")] 
    (if (nil? (re-matches p n))
      false
      true)))

(def assign-pattern #"\w+=\d+")
(println (re-matches assign-pattern "foo=123"))         ;; true
(println (re-find assign-pattern "foo=123, bar=456"))   ;; foo=123
(println (re-seq assign-pattern "foo=123, bar=456"))  ;; (foo=123 bar=456)

(java.lang.String/valueOf 1234)
(java.lang.String/valueOf 1234.5)

(ns main_namespace)   ;; create and switch to new namespace
(println *ns*)        ;; current namespace
(def t 2)             ;; bind 2 to t
(println t)           ;; print 2
(ns user)             ;; switch back
(println *ns*)        ;; current namespace
(def t 1)             ;; bind 1 to t
(println t)           ;; print 1

(println main_namespace/t)  ;; full reference
(println user/t)            ;; full reference

;; CLASSPATH="./"  main.clj
(import 'Foo)             ;; from Foo.java
(def f1(Foo.))            ;; instance of Foo (syntax 1)
(def f2(new Foo))         ;; instance of Foo (syntax 2)
(println (Foo/f_static 1 6))  ;; call static method on Foo
(println (.f f1 1 6))         ;; call non-static method on instance
(println (Foo$InnerFoo/data))   ;; access to static variable of inner class
(import 'Foo$InnerFoo)          ;; from Foo.java
;; (println (Foo$InnerFoo.))       ;; seems not to work


(use 'clojure.reflect)    ;; reflecton
(use 'clojure.pprint)     ;; pretty print
(println (reflect *ns*))
;; (println ((reflect *ns*) :members))

