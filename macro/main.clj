#!/usr/bin/env -S clojure -cp "."

(def a (for[i (range 10) j (range 10)] (list i j)))
(def b (for[i (range 10) j (range 10)] `(~i ~j)))
(println a)
(println "(= a b)" (= a b))

(defrecord R[x y z])
(def r (R. 1 2 3))
(:x r)
(.x r)
(. r x)

(defmacro unless1[cond expr1 expr2] `(if (not ~cond) ~expr1 ~expr2))
(defmacro unless2[cond expr1 expr2] `(do (let[ms# (System/currentTimeMillis)] (println "running " ms#))(if (not ~cond) ~expr1 ~expr2)))
(defmacro addup[& vals] `(+ ~@vals))

(defmacro macro 
  ([] 0)
  ([x] 1)
  ([x y] 2))

(defmacro apply-op[op x y] `(~op ~x ~y))
(apply-op + 10 5)

;; returns 5
(apply-op '+ 10 5) 
;; ((quote +) 10 5) => 5
;; ('foo 10 5) => 5
(macroexpand '(apply-op '+ 10 5))

(println "(type []) =>" (type []))
(println "(type '[]) =>" (type '[]))
(println "(type `[]) =>" (type `[]))

(println "(type 1) =>" (type 1))
(println "(type '1) =>" (type '1))
(println "(type `1) =>" (type `1))

(ifn? 1)
(ifn? "")
(ifn? '+)
(ifn? 'foo)
(ifn? :foo)

;; return 1
(get [1 2 3 4] 1)
;; return nil
(get [1 2 3 4] 10)
;; return 0
(get [1 2 3 4] 10 0)
;;
;; returns 1
(get {:x 1 :y 2} :x)
;; returns nil
(get {:x 1 :y 2} :z)
;; returns 0
(get {:x 1 :y 2} :z 0)
;; key as function
(:x {:x 1 :y 2})
(:z {:x 1 :y 2})
(:z {:x 1 :y 2} 0)

(+ '1 1)
(+ `1 1)
(+ ``1 1)
(+ ```1 1)
(+ ````1 1)
(+ ````~~~~1 1)
(+ ``~``1 1)

(when true 1 2 3 4)
(clojure.core/when  1 2 3 4)

;; special forms are evaluated with rules that differ from standard clojure evaluation
;; special forms are understood by clojure compiler directly
;; if
;; def

;; macroexpand -> repeatedly calls macroexpand-1
(defmacro m[a b c d & xs] 1)
(macroexpand-1 '(m 1 2 3 4 5))

;; defmacro sets ^{:macro true}
;; defn has no :macro key
;;
;; quote '
;; syntax quote `
;; both may be used in defn and defmacro
;;
;; `name will be resolved to fully qualified name of current namespace
;; `~name will be replaced with value
;; `~'name not resolved, remains symbol
(def x 100)
(defmacro add1[x] (list '(+ 1 x) `(+ 1 x) `(+ 1 ~x)))
(println (macroexpand-1 '(add1 0)))
(defmacro add2[x] ['(+ 1 x) `(+ 1 x) `(+ 1 ~x)])
(println (macroexpand-1 '(add2 0)))
(defmacro add3[x] ['(+ 1 x) `(+ 1 x) `(+ 1 ~x) `(+ 1 ~'x)])
(println (macroexpand-1 '(add3 0)))

