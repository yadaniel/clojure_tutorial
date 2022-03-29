(ns main)

(defrecord R [a b c d])
(def r1 (R. 1 2 3 4))
(println (type R))
(println r1)
