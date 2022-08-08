#!/usr/bin/env -S clojure -cp "."

(defn f
  ([] (println "no parameters"))
  ([x] (println "one ignored parameter => " (str x)))
  ([x y] (+ x y))
  ([x y & z] (apply + z)))

(defn arity-of [f] (count (:arglists (meta (var f)))))
(println "arity of f => " (arity-of f))

(println "arity of f => " (count (:arglists (meta (var f)))))
(println "arglists of f => " (:arglists (meta (var f))))

(sort '(5 4 3 2 1))
(sort [5 4 3 2 1])

(sorted-map-by < 10 0, 9 1, 8 2, 7 3, 6 4, 5 5)

(sort-by first [[0 10 100] [2 200 29] [1 2 1000]])
(sort-by second [[0 10 100] [2 200 29] [1 2 1000]])
(sort-by last [[0 10 100] [2 200 29] [1 2 1000]])

(sort-by #(nth % 0) [[0 10 100] [2 200 29] [1 2 1000]])
(sort-by #(nth % 1) [[0 10 100] [2 200 29] [1 2 1000]])
(sort-by #(nth % 2) [[0 10 100] [2 200 29] [1 2 1000]])

(sort-by :id [{:id 3} {:id 2} {:id 1}])
(sort-by :id [{:id 3, :val 10} {:id 2, :val 0} {:id 1, :val 7}])
(sort-by :val [{:id 3, :val 10} {:id 2, :val 0} {:id 1, :val 7}])
(sort-by :val [{:id 3, :val 10} {:id 2, :val 0} {:id 1, :val 7}])
(sort-by :val (list {:id 3, :val 10} {:id 2, :val 0} {:id 1, :val 7}))
(sort-by :val (sort-by :id (list {:id 3, :val 10} {:id 2, :val 0} {:id 1, :val 7})))

(Byte/MIN_VALUE)
(Byte/MAX_VALUE)
(Short/MIN_VALUE)
(Short/MAX_VALUE)
(Integer/MIN_VALUE)
(Integer/MAX_VALUE)
(Long/MIN_VALUE)
(Long/MAX_VALUE)

(Float/MIN_VALUE)
(Float/MAX_VALUE)
(Double/MIN_VALUE)
(Double/MAX_VALUE)

;; BigInteger 1N
;; BigDecimal 1M

(meta #'juxt)
(meta (var juxt))
(def ^{:doc "usage of my var"} x 1)

((juxt #(Math/sin %) #(Math/cos %)) 0)
((juxt :id :val) {:val 100, :doc "something", :id 0})

(import java.time.LocalTime)
(str (LocalTime/now))

(import java.time.LocalDate)
(str (LocalDate/now))

(import java.time.LocalDateTime)
(str (LocalDateTime/now))

(import java.time.format.DateTimeFormatter)
(def dt_fmt (DateTimeFormatter/ofPattern "dd-MM-yyyy"))
(def dt_now (LocalDateTime/now))
(.format dt_now dt_fmt)
(. dt_now format dt_fmt)



