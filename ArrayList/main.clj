#!/usr/bin/env clojure

(import java.util.ArrayList)

(def al0 (ArrayList.))
(def al1 (ArrayList. 10))     ;; initial capacity

;; empty
(println "(count al0)" (count al0))
(println "(count al1)" (count al1))
(println "(.size al0)" (.size al0))
(println "(.size al1)" (.size al1))
(println "(.isEmpty al0)" (.isEmpty al0))
(println "(.isEmpty al1)" (.isEmpty al1))

;; generic get
(println "(get al0 0)" (get al0 0))       ;; nil
(println "(get al1 0)" (get al1 0))       ;; nil
(println "(get al0 0 0)" (get al0 0 0))   ;; 0
(println "(get al1 0 0)" (get al1 0 0))   ;; 0
(println)

(.add al0 1)
(.add al1 1)

;; non empty
(println "(count al0)" (count al0))
(println "(count al1)" (count al1))
(println "(.size al0)" (.size al0))
(println "(.size al1)" (.size al1))
(println "(.isEmpty al0)" (.isEmpty al0))
(println "(.isEmpty al1)" (.isEmpty al1))

;; generic get
(println "(get al0 0)" (get al0 0))       ;; nil 
(println "(get al1 0)" (get al1 0))       ;; nil
(println "(get al0 0 0)" (get al0 0 0))   ;; 0
(println "(get al1 0 0)" (get al1 0 0))   ;; 0
(println)

;; with values
(def al2 (ArrayList. [10]))
(def al3 (ArrayList. (range 10)))
(print al2)
(print al3)
(println al2)
(println al3)
(printf "%s\n" al2)
(printf "%s\n" al3)
(println (str al2))
(println (str al3))

;; generic get => convert to array
(get al3 0)                         ;; nil
(get al3 0 "default")               ;; "default"
(get (.toArray al3) 0)              ;; 0
(get (.toArray al3) 100)            ;; nil
(get (.toArray al3) 100 "default")  ;; default

(loop [i 10 acc []]
  (if (not= 0 i) 
    (recur (dec i) (conj acc (java.util.Date.)) ) 
    acc))

(loop [i 10 acc []]
  (if (= 0 i) 
    acc
    (recur (dec i) (conj acc (java.util.Date.)))))

(def xs (ArrayList. 10))
(dotimes [_ 10] (.add xs (java.util.Date.)))


