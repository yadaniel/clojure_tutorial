#!/usr/bin/env cljbbw.exe
;;#/usr/bin/env cljbb.exe
;;#/mnt/c/bin/cljbb.exe

;; (require '[clojure.tools.cli :as cli])
;; (require '[babashka.cli :as cli_])
;; (require '[babashka.fs :as fs])

;; keys are passed first
(condp (fn[x y](pr x) false) 0
    1 'a 
    2 'b 
    3 'c 
    4 'd 
    "")

(def v (condp = (read)
         1 "eins"
         2 "zwei"
         3 "drei"
         4 "vier"
         ""))

(println v)
;; (flush)
;; (newline)

(def w (let [w' (read)]
         (cond
           (= w' 1) "eins"
           (= w' 2) "zwei"
           (= w' 3) "drei"
           (= w' 4) "vier"
           true "")))

(println w)
;; (flush)
;; (newline)

(def l' (atom []))
(swap! l' conj 1)
(swap! l' (fn[x](conj x 2)))
(pr @l')
(newline)

(def ai (atom 0))
(swap! ai inc)
(swap! ai (fn[x](+ x 2)))
(pr @ai)
(newline)

