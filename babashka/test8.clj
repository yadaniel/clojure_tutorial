#!/usr/bin/env cljbbw.exe

; comment
;; comment 
;;; comment
;;;; comment

(comment)
(comment "text")
(comment (1 2 3 4))

(def x (* 1 2 3 4 5 6 7 #_8 9 10))
(println x)

(def y (* 1 2 3 4 5 6 7 #_(+ 4 4) 9 10))
(println y)

