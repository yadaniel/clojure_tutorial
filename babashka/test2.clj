#!/usr/bin/env cljbbw.exe
;;#/usr/bin/env cljbb.exe
;;#/mnt/c/bin/cljbb.exe

;; das ist ein test, auch mit Nummer wie 1, 2 oder auch 345

(def cnt (atom 0))
(defn cnt-inc [] (swap! cnt inc))
(def up (repeatedly cnt-inc))
(prn (take 10 up))
(prn (take 10 (drop 10 up)))

(prn (for [i "abcd", j (range 4)] `(~i ~j)))
(prn (into [] (for [i "abcd", j (range 4)] `(~i ~j))))
(prn (for [i "abcd", j (range 4)] (vector i  j)))
(prn (into [] (for [i "abcd", j (range 4)] (vector i  j))))

(defmacro e [f] (list (nth f 1) (nth f 0) (nth f 2)))
(prn (e (2 + 3)))
(prn (e (2 * 3)))

(defmacro e [f] `(~(nth f 1) ~(nth f 0) ~(nth f 2)))
(prn (e (2 + 3)))
(prn (e (2 * 3)))

;; (defn f[x y] (let [x' (or x 0), y' (or y 0)] (+ x' y')))
(defn f [x y] (let [x' (if (number? x) x 0), y' (if (number? y) y 0)] (+ x' y')))
(prn (f nil nil))
(prn (f 1 nil))
(prn (f [] nil))
(prn (f 2 []))

(when true (prn "yes"))
(if true (prn "yes") nil)

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


