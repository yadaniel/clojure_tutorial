#!/usr/bin/env clojure

(ns examples)

(require 'clojure.reflect)

(def v (vector [1 2 3 4 4.5] [5 6 7 8] [] [[9]]))
(println "v =>" v)
(println "flatten =>" (flatten v))
(println "reverse =>" (reverse v))
(println "count flatten =>" (count (flatten v)))
(println "count =>" (count v))
(println "sort =>" (sort v))        ;; based on count

;; conj with vector appends at tail
(println (conj []))
(println (conj [1]))
(println (conj [] 1))
(println (conj [] 1 2))
(println (conj [] 1 2 3))
(println (conj [1 2 3] 4 5 6))

;; conj with list prepends at head
(println (conj '()))
(println (conj '(1)))
(println (conj '() 1))
(println (conj '() 1 2))
(println (conj '() 1 2 3))
(println (conj '(1 2 3) 4 5 6))

;; conj with map => no ordering within map
(println (conj {}))
(println (conj {} [1 "one"]))
(println (conj {2 "two"} [1 "one"]))

;; conj with set => no ordering within set
(println (conj #{}))
(println (conj #{} 1 2))
(println (conj #{1 2} 10))
(println (conj (set [])))
(println (conj (set []) 1 2))
(println (conj (set [1 2]) 10))

;; (println (for [i (clojure.reflect/reflect "")] (format "%s\n" i)))
(def rs (clojure.reflect/reflect (String.)))
(println "bases =>" (:bases rs))
(println "members =>"(:members rs))
(map #(println % "\n") (:members rs))

;; :tag
;; :doc
;; :sig
;; :arglists
(defn ^Integer f "function f with 0 or 2 parameters" {:sig "int->int->int|none"} ([x y] (+ x y)) ([] 0))
(println "(f) => " (f))
(println "(f 1 2) => " (f 1 2))
;; (println "(doc f) => " (doc f))
;; (println "(doc f) => " (:doc (meta (var f))))

(println "nth ...")
(time (nth (cycle (range 101)) 1000))
(clojure.core/time (nth (cycle (range 99)) 1001))

;; nth
(println (nth [0 1 2 3] 0))
(println (clojure.core/nth [0 1 2 3] 0))
(println (nth [] 0 -1))

;; with-meta seems not to work with 1 ""
(def xs (with-meta [1 2 3 4] {:info "vector used for counts"}))
(println "(meta x) => " (:info (meta xs)))
(println "(meta (var x)) => " (meta (var xs)))

;; find out the order of function arguments
(reduce (fn [acc x](+ (* 2 acc) x)) 1 [0])
(reduce (fn [acc x](do (println "acc => " acc) (+ acc x))) 1 [0])

(defn sum-reduce[xs]
    (reduce (fn [acc x](+ acc x)) 0 xs))

;; dummy parameter
(defn sum-reduce'
    ([xs](reduce (fn [acc x](+ acc x)) 0 xs))
    ([xs t](reductions (fn [acc x](+ acc x)) 0 xs)))

(defn sum-reduce''[xs t]
    (cond
        (= t false) (reduce (fn [acc x](+ acc x)) 0 xs)
        (= t true) (reductions (fn [acc x](+ acc x)) 0 xs)))

(println "rationalize (+ 1/10 1/10) after cast to float => " (rationalize (float (+ 1/10 1/10))))
(println "rationalize (+ 1/10 1/10) after cast to douible => " (rationalize (double (+ 1/10 1/10))))

(cons 3 (cons 2 (cons 1 (cons 0 []))))
(cons 3 (cons 2 (cons 1 (cons 0 '()))))
(conj (conj (conj (conj [] 0) 1) 2) 3)
(assoc (assoc (assoc (assoc {} 1 2) 3 4) 5 6) 7 8)
(dissoc (dissoc {0 false 1 2 3 4 5 6 7 8 9 true} 0) 9)

;; persistent-hash-set
;; #{1 2 3 4} ok
;; #{1 2 3 4 4} error
(= #{1 2 3 4} (hash-set 1 2 3 4 4))
(= #{1 2 3 4} (set [1 2 3 4 4]))
;; persistent-tree-set
(sorted-set 1 2 3 4)
;;
(disj #{1 2 3 4 5} 5)
(disj (conj #{1 2 3 4 5} 5))

;; vector and list
(nth [1 2 3 4] 0)
(nth [1 2 3 4] 0)

;; array
(def xs (int-array 10 1))
(aget xs 0)
(aset xs 0 2)
(aget xs 0)

;; conversion
(def ys0 (into-array (range 10)))
(def ys1 (into-array (list 1 2 3 4)))
(def ys2 (into-array (vector 1 2 3 4)))

;; every? ... names with ? return true|false
;; some ... returns true|nil
(some (fn[x](= x 10)) [1 2 3 4])
(some (fn[x](not= x 0)) [1 2 3 4])
(every? (fn[x](not= x 0)) [1 2 3 4])

(defn adder[x](fn [y](+ x y)))
(def adder10 (adder 10))
(adder10 1)

;; (cast Integer 1)
(cast Double 1.0)
(cast Integer nil)
(cast Double nil)
(println "Character/TYPE =>" Character/TYPE)
(println "Boolean/TYPE =>" Boolean/TYPE)
(println "Integer/TYPE =>" Integer/TYPE)
(println "Double/TYPE =>" Double/TYPE)
(println "Float/TYPE =>" Float/TYPE)

;; apply takes function and args and "rewrites" it
;; (f args)
;; (apply f [args])
(apply + [1 2 3 4])
(apply * [1 2 3 4])
(apply get [[1 2 3 4] 0])

;; in-range
(defn in-range?[low high value](<= low value high))
#!/usr/bin/env clojure

(ns examples)

(require 'clojure.reflect)

(def v (vector [1 2 3 4 4.5] [5 6 7 8] [] [[9]]))
(println "v =>" v)
(println "flatten =>" (flatten v))
(println "reverse =>" (reverse v))
(println "count flatten =>" (count (flatten v)))
(println "count =>" (count v))
(println "sort =>" (sort v))        ;; based on count

;; conj with vector appends at tail
(println (conj []))
(println (conj [1]))
(println (conj [] 1))
(println (conj [] 1 2))
(println (conj [] 1 2 3))
(println (conj [1 2 3] 4 5 6))

;; conj with list prepends at head
(println (conj '()))
(println (conj '(1)))
(println (conj '() 1))
(println (conj '() 1 2))
(println (conj '() 1 2 3))
(println (conj '(1 2 3) 4 5 6))

;; conj with map => no ordering within map
(println (conj {}))
(println (conj {} [1 "one"]))
(println (conj {2 "two"} [1 "one"]))

;; conj with set => no ordering within set
(println (conj #{}))
(println (conj #{} 1 2))
(println (conj #{1 2} 10))
(println (conj (set [])))
(println (conj (set []) 1 2))
(println (conj (set [1 2]) 10))

;; (println (for [i (clojure.reflect/reflect "")] (format "%s\n" i)))
(def rs (clojure.reflect/reflect (String.)))
(println "bases =>" (:bases rs))
(println "members =>"(:members rs))
(map #(println % "\n") (:members rs))

;; :tag
;; :doc
;; :sig
;; :arglists
(defn ^Integer f "function f with 0 or 2 parameters" {:sig "int->int->int|none"} ([x y] (+ x y)) ([] 0))
(println "(f) => " (f))
(println "(f 1 2) => " (f 1 2))
;; (println "(doc f) => " (doc f))
;; (println "(doc f) => " (:doc (meta (var f))))

(println "nth ...")
(time (nth (cycle (range 101)) 1000))
(clojure.core/time (nth (cycle (range 99)) 1001))

;; nth
(println (nth [0 1 2 3] 0))
(println (clojure.core/nth [0 1 2 3] 0))
(println (nth [] 0 -1))

;; with-meta seems not to work with 1 ""
(def xs (with-meta [1 2 3 4] {:info "vector used for counts"}))
(println "(meta x) => " (:info (meta xs)))
(println "(meta (var x)) => " (meta (var xs)))

;; find out the order of function arguments
(reduce (fn [acc x](+ (* 2 acc) x)) 1 [0])
(reduce (fn [acc x](do (println "acc => " acc) (+ acc x))) 1 [0])

(defn sum-reduce[xs]
    (reduce (fn [acc x](+ acc x)) 0 xs))

;; dummy parameter
(defn sum-reduce'
    ([xs](reduce (fn [acc x](+ acc x)) 0 xs))
    ([xs t](reductions (fn [acc x](+ acc x)) 0 xs)))

(defn sum-reduce''[xs t]
    (cond
        (= t false) (reduce (fn [acc x](+ acc x)) 0 xs)
        (= t true) (reductions (fn [acc x](+ acc x)) 0 xs)))

(println "rationalize (+ 1/10 1/10) after cast to float => " (rationalize (float (+ 1/10 1/10))))
(println "rationalize (+ 1/10 1/10) after cast to douible => " (rationalize (double (+ 1/10 1/10))))

(cons 3 (cons 2 (cons 1 (cons 0 []))))
(cons 3 (cons 2 (cons 1 (cons 0 '()))))
(conj (conj (conj (conj [] 0) 1) 2) 3)
(assoc (assoc (assoc (assoc {} 1 2) 3 4) 5 6) 7 8)
(dissoc (dissoc {0 false 1 2 3 4 5 6 7 8 9 true} 0) 9)

;; persistent-hash-set
;; #{1 2 3 4} ok
;; #{1 2 3 4 4} error
(= #{1 2 3 4} (hash-set 1 2 3 4 4))
(= #{1 2 3 4} (set [1 2 3 4 4]))
;; persistent-tree-set
(sorted-set 1 2 3 4)
;;
(disj #{1 2 3 4 5} 5)
(disj (conj #{1 2 3 4 5} 5))

;; vector and list
(nth [1 2 3 4] 0)
(nth [1 2 3 4] 0)

;; array
(def xs (int-array 10 1))
(aget xs 0)
(aset xs 0 2)
(aget xs 0)

;; conversion
(def ys0 (into-array (range 10)))
(def ys1 (into-array (list 1 2 3 4)))
(def ys2 (into-array (vector 1 2 3 4)))

;; every? ... names with ? return true|false
;; some ... returns true|nil
(some (fn[x](= x 10)) [1 2 3 4])
(some (fn[x](not= x 0)) [1 2 3 4])
(every? (fn[x](not= x 0)) [1 2 3 4])

(defn adder[x](fn [y](+ x y)))
(def adder10 (adder 10))
(adder10 1)

;; (cast Integer 1)
(cast Double 1.0)
(cast Integer nil)
(cast Double nil)
(println "Character/TYPE =>" Character/TYPE)
(println "Boolean/TYPE =>" Boolean/TYPE)
(println "Integer/TYPE =>" Integer/TYPE)
(println "Double/TYPE =>" Double/TYPE)
(println "Float/TYPE =>" Float/TYPE)

;; apply takes function and args and "rewrites" it
;; (f args)
;; (apply f [args])
(apply + [1 2 3 4])
(apply * [1 2 3 4])
(apply get [[1 2 3 4] 0])

;; empty or even number of forms
(cond)
(cond 
  nil 1
  nil 2
  nil 3
  nil 4)
(cond 
  nil 1
  nil 2
  nil 3
  nil 4
  :else 10)

(defn in-range?[low high value](<= low value high))
(println "5 in range between 0 and 10 =>" (in-range? 0 10 5))

(re-matches #"(\w+)=(\d+)" "foo=1234 ")
(re-find #"(\w+)=(\d+)" "foo=1234 ")
(re-seq #"(\w+)=(\d+)" "foo=1234 bar=1234")

(condp = 0 0 "got 0" 1 "got 1" "something else")
(condp = 1 0 "got 0" 1 "got 1" "something else")
(condp = 10 0 "got 0" 1 "got 1" "something else")

;; condp is a function of 2 parameters
;; first parameter is supplied directly
;; second parameter is supplied from the corresponding line
(condp #(= %1 %2) 1 
  1 "eins"
  2 "zwei"
  3 "drei"
  4 "vier")


