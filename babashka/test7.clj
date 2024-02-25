#!/usr/bin/env cljbbw.exe

(defmacro m [op] `(~op 1 2 3 4))
(println (m +))
(println (m *))

(def y 10)
(defmacro m2 [x] `(+ ~x ~y))
(println (m2 1))

(def xs (list 1 2 3 4))
(defmacro m3 [x] `(+ ~@xs))
(println (m3 xs))
(println (m3 '(1 2 3 4)))

(defmacro m4 [x & xs]
  (println "form => " &form)
  (println "env => " &env)
  `(~x ~@xs))

(println (m4 + 1 2 3 4))

(defmacro m5 [x & xs]
  (let [i 0]
    (println "form => " &form)
    (println "env => " &env)
    (println "let => " i)
    `(~x ~@xs)))

(println (m5 + 1 2 3 4))
(let [j 0] (println (m5 * 1 2 3 4)))
(let [x 10, y 20] (println (m5 * 1 2 3 4)))

