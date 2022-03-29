#!/usr/bin/env -S clojure -cp "."

(println "start")

(import A)

;; static
(println "(A/as)" (A/as))
(println "(A/afs)" (A/afs))
(println "(A/afs1 1)" (A/afs1 1))

;; instance
(def b(A.))
(println "(. b a)" (. b a))
(println "(. b af)" (. b af))
(println "(. b af 1)" (. b af1 1))

(println "(.a b)" (.a b))
(println "(.af b)" (.af b))
(println "(.af1 b 1)" (.af1 b 1))

(println "(.. b self self self self)" (.. b self self self self))
(println "(.. b (self_param 1) (self_param 2) (self_param 3) (self_param 4))" (.. b (self_param 1) (self_param 2) (self_param 3) (self_param 4)))
(println "(.cnt b)" (.cnt b))

;; add val => push
;; add idx val => insert at idx
(import java.util.ArrayList)
(def al (ArrayList.))
(doto al (.add 1) (.add 2) (.add 3) (.add 4))
(println "(todo (ArrayList.) (.add 1) (.add 2) (.add 3) (.add 4)) =>" al)
(println "(.size al)" (.size al))
(.clear al)
(println "(.size al)" (.size al))
(doto al (.add 1)(.add 2)(.add 3)(.add 4))
(doto al (.add 0 10)(.add 0 20)(.add 0 30)(.add 0 40))
(println al)
(.addAll al '(1 2 3 4))
(.addAll al [1 2 3 4])
(println al)
(println "(.contains al 1)" (.contains al 1))
(println "(.contains al 5)" (.contains al 5))
;; remove first 1 => true if removed
(println "(.remove al 1)" (.remove al 1))
;; remove first 1000 => true if removed
(println "(.remove al 1000)" (.remove al 1000))

(import java.util.Stack)
(def s (Stack.))
(println s)
(doto s (.push 1) (.push 2) (.push 3) (.push 4) (.push 5))
(println (.pop s))
(println s)
(.clear s)
(println s)
(def s1 (doto (Stack.) (.push 100) (.push 200)))

(require 'clojure.pprint)
(clojure.pprint/pprint s1)




