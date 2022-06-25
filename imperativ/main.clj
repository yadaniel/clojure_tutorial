#!/usr/bin/env clojure

(require '[clojure.pprint :as p])
(p/write 100 :base 2)
(p/write 100 :base 16)
(p/write 100 :base 2 :radix true)
(p/write 100 :base 16 :radix true)

(print "in main")
(println "in main")
(printf "%s" "in main")
(printf "%s\n" "in main")

(def a0 1)
(def a1 (atom 1))

;; same type
(print (type 1))
(print (type a0))
(print (type @a1))
(print (type a1))

(let [run (atom true)] 
  (while @run 
    (reset! run false) 
    (print (java.util.Date.))))

;; classical for loop
(let [i (atom 0) n 20]
  (while (< @i n) 
    (printf "iteration [%s,%s,%s]\n" @i n (int (/ (System/currentTimeMillis) 1000)))
    (flush)
    (Thread/sleep 100)
    (reset! i (inc @i))))

(def cnt (atom 0))
(defn up[] (swap! cnt #(inc %)))
(defn down[] (swap! cnt #(dec %)))
(defn shift-by[offset] (swap! cnt #(+ % offset)))
(dotimes [_ 10] (up))
(println "cnt up => " @cnt)
(loop [i 0] (if (< i 10) (do (down)(recur (inc i)))))
(println "cnt down => " @cnt)
(def cnt' (for[i (range 5)] (shift-by i)))
;; (def cnt' (doall (for[i (range 5)] (shift-by i))))
;; (def cnt' (vec (for[i (range 5)] (shift-by i))))
;; (def cnt' (list (for[i (range 5)] (shift-by i))))
;; (def cnt' [(for[i (range 5)] (shift-by i))])
(println "cnt shift1 => " @cnt)
(println "cnt' => " cnt')
(println "cnt shift2 => " @cnt)

(def bool (atom false))
(swap! bool (fn[x](not x)))
(swap! bool (fn[x](not x)))
(swap! bool (fn[x](not x)))
(swap! bool (fn[x](not x)))
(swap! bool #(not %))
(swap! bool #(not %))
(swap! bool #(not %))
(swap! bool #(not %))

(def aa (atom (atom 0)))
(println "aa => " aa)
(println "@aa => " @aa)
(println "@@aa => " @@aa)

(defn just-one[] 1)

(def ones (take 10 (repeat 1)))
(println "ones => " ones)
(def ones' (for[i (range 10)]1))
(println "ones' => " ones')
(def ones'' (for[i (range 10)]((fn[]1))))
(println "ones'' => " ones'')
(def ones''' (for[i (range 10)](just-one)))
(println "ones''' => " ones''')

(def x (atom 1))
(defn mult[i] (swap! x #(* i %)))
(def t' (for[i (range 1 10)] (mult i)))
(println "x 1 => " @x)
(println "t' => " t')
(println "x 2 => " @x)

(def y (atom 0))
(defn shiftby[offset] (swap! y #(+ offset %)))
;; (defn shiftby[offset] (swap! y #(+ % offset)))
(def y' (for[i (range 5)] (shiftby i)))
(println "y shift1 => " @y)
(println "y' => " y')
(println "y shift2 => " @y)

