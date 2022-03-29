#!/usr/bin/env clojure

(ns types)

(import java.lang.String)
(import java.lang.StringBuilder)
(import java.lang.Byte)
(import java.lang.Integer)
(import java.lang.Double)
(import java.lang.Boolean)
(import java.math.BigDecimal)
(import java.math.BigInteger)
(import java.util.Date)
(import java.util.Calendar)
(import java.util.TimeZone)
(import java.time.MonthDay)
(import java.io.File)
(import java.nio.file.Path)
(import java.util.Scanner)
(import java.lang.Math)
(import java.lang.StrictMath)

;; required by default
;; clojure.core/int
;; clojure.core/double
;; repl will load the following
;; (clojure.repl/dir clojure.core)
(require 'clojure.core)
(require 'clojure.repl)
(println "..." (clojure.repl/dir types))
(read-line)

;; preferred
(def b0 (byte 1))
(def s0 (short 1))
(def i0 (int 1))
(def l0 (long 1))
(def f0 (float 1.0))
(def d0 (double 1.0))
(println "clojure objects" b0 s0 i0 l0 f0 d0)

;; java objects
(def b1 (Byte. "1"))
(def s1 (Short. "1"))
(def i1 (Integer. "1"))
(def l1 (Long. "1"))
(def f1 (Float. "1.0"))
(def f2 (Float. "1.0f"))
(def d1 (Double. "1.0"))
(def d2 (Double. "1.0f"))
(println "java objects" b1 s1 i1 l1 f1 f2 d1 d2)

;; java objects
(def b2 (new Byte "1"))
(def s2 (new Short "1"))
(def i2 (new Integer "1"))
(def l2 (new Long "1"))
(def f3 (new Float "1.0"))
(def f4 (new Float "1.0f"))
(def d3 (new Double "1.0"))
(def d4 (new Double "1.0f"))
(println "java objects (new)" b2 s2 i2 l2 f3 f4 d3 d4)

(def xs (list
  (BigDecimal. 1234.1234)     ;; => 1M
  (BigDecimal. "1234.1234")   ;; => 1M
  (BigInteger. "1")           ;; => 1N
))
(println xs)
(println)

(refer 'clojure.repl)
(println "dir ..." (dir types))
(println "dir ..." (clojure.repl/dir types))
(read-line)

;; meta
(defn f[x y](+ x y))
(println "meta objects of function f: " (meta #'f))     ;; #'sym expands to (var sym)
(println "function f takes: " (:arglists (meta #'f)))
(println)

;; meta dictionary with custom fields
(defn q {:docstring "function q adds two numbers"} [x y] (+ x y))
(println "function q docstring: " (:docstring (meta #'q)))
(println)

;; meta dictionary with available doc
(defn r "function r adds two numbers" [x y] (+ x y))
(println "meta of r: " (meta #'r))
(println "function r docstring: " (:doc (meta #'r)))
(println)

;; meta with 
(defn ^long s "docstring of s" {:info "howto"} [^double x ^double y] (long (+ x y)))
(println "meta of s: " (meta #'s))
(println "info of s: " (:info (meta #'s)))
(println)

;; aquivalent to setting private:true
(defn- f_private[])
(println "meta of f_private: " (meta (var f_private)))
(println)

;;

(type Math/PI)
(type Math/E)
(type (Math/sin 0))

(set [1 2 3 4])
(hash-map 1 2 3 4)
(list 1 2 3 4)
(vector 1 2 3 4)
(defrecord vec2d[x y])

(def origin1 (vec2d. 0 0)) 
(def origin2 (new vec2d 0 0))
(println "type of vec2d =>" (type vec2d))
(println "type of Byte =>" (type Byte))
(read-line)

(def inttypes
  [(Byte/MIN_VALUE) (Byte/MAX_VALUE)
   (Short/MIN_VALUE) (Short/MAX_VALUE)
   (Integer/MIN_VALUE) (Integer/MAX_VALUE)
   (Long/MIN_VALUE) (Long/MAX_VALUE)]
)
(println types/inttypes)

(def floattypes
  [(Float/MIN_VALUE) (Float/MAX_VALUE)
   (Double/MIN_VALUE) (Double/MAX_VALUE)]
)
(println types/floattypes)

(def msg1 "foo")
(def msg2 (String. "bar"))
(println msg1 "=>" (.length msg1))
(println msg2 "=>" (.length msg2))
(println msg1 "=>" (. msg1 length))
(println msg2 "=>" (. msg2 length))
(println "(Math/PI)" (Math/PI))
(println "(. Math)" (Math/PI))
(println (.charAt "foo" 0))
(println (. "foo" charAt 0))
(println (= (String/valueOf 1234) (. String valueOf 1234)))

(.concat "1" (String. "2"))
(.concat (String. "2") "1")
(.concat "1" "2")
(. "1" concat "2")

(def ^Double msg_bytes (. "abcd" getBytes))
(count msg_bytes)

(char-array 10)
(byte-array 10)
(short-array 10)
(int-array 10)
(long-array 10)
(float-array 10)
(double-array 10)
(def ia10 (int-array 10))
(count ia10)
(def ia10_init_def (int-array 10 1))
(def ia10_init_vec (int-array [1 2 3 4 5 6 7 8 9 10]))

(subvec [1 2 3 4] 0 4)
(contains? [1 2 3 4] 1)
(make-array Double 2)
(make-array Double 2 2)
(make-array Double 2 2 2)
(make-array Double/TYPE 2)
(make-array Double/TYPE 2 2)
(make-array Double/TYPE 2 2 2)
(def d2x2 (make-array Double 2 2))
(def d2x2 (make-array Double/TYPE 2 2))
(aget d2x2 0 0)
(aget d2x2 0 1)
(aget d2x2 1 0)
(aget d2x2 1 1)
(aset d2x2 0 0 100)
(aset d2x2 0 1 200)
(aset d2x2 1 0 300)
(aset d2x2 1 1 400)
(aget d2x2 0 0)
(aget d2x2 0 1)
(aget d2x2 1 0)
(aget d2x2 1 1)

(take 10 (range 100))
(drop 10 (range 100))
(take-last 10 (range 100))
(drop-last 10 (range 100))
(first (range 100))
(last (range 100))
(rest (range 100))
(def c (cycle '(1 2 3 4)))
(take 10 c)
(take 10 (repeat 1))

(.toString (Date.))
(println "pwd => " (.getCanonicalPath (File. ".")))

;; 
(println "types => " (map #(type %) (list \A 1 1.0 1N 1M)))
(println "types => " (map #(class %) (list \A 1 1.0 1N 1M)))

(loop [i 5] (do (print "line " i ",\n")
                 ;; (read-line)
                 (flush)
                 (if (= i 0) nil (recur (dec i)))))

(loop [i 5] (do (print "line " i ",\n")
                 ;; (read-line)
                 (flush)
                 (if (> i 0) (recur (dec i)))))

;; ref
(def rval (ref 0))
(println "rval =>" (deref rval))
(println "rval =>" @rval)
(dosync (ref-set rval (inc (deref rval))))
(println "rval =>" (deref rval))

;; unbound values
(declare ys)
(def zs)

; false | true
(def t0 (Boolean. "true"))
(def f0 (Boolean. ""))
(def t1 (boolean 1))
(def f1 (boolean nil))
;; note, that (Boolean. everything-false-but-string-"true"-case-not-relevant)
;; note, that (boolean, everthing-true-but-nil)

;; use = require, refer
(use 'clojure.pprint)
(def a0 (atom 1))
(pprint a0)
(reset! a0 0)
(pprint a0)

(def sb(StringBuilder.))
(.length sb)
(.capacity sb)
(.append sb "foo")
(.toString sb)
(.append sb "bar")
(.toString sb)
(.append sb "1234567890")
(.capacity sb)
(.append sb "a")
(.capacity sb)
(.append sb \a)
(.capacity sb)
(.toString sb)
(.reverse sb)
(.toString sb)
(.append sb (int 1))
(.append sb (long 1))

;; (Character/toChars 48)
;; codePoint -> UTF16

(= (Math/sin 0) (StrictMath/sin 0))
(not= (Math/sin 0) (StrictMath/sin Math/PI))

(println "function block")
(interpose 0 [1 2 3 4])
(interpose \space "foobar")
(interleave [0 0 0 0] [1 1 1 1] [2 2 2 2] [3 3 3 3] [4 4 4 4])
(interleave "foo" "bar" "baz")
(def ones (repeat 1))
(def onetwo (cycle [1 2]))
(def randints (repeatedly #(rand-int 100)))
(def randdoubles (repeatedly #(Math/random)))
(def randdoubles2 (repeatedly (fn[](Math/random))))
;; in script not evaluated
(repeat 1)
(cycle [1 2])
;; (doall (repeat 1))
;; (doall (cycle [1 2]))
(println "function block ends")

;; flush is needed, "\n" does not flush
(dotimes [i 10] (do (printf "[%d/%d] = %s\n" i 10 i) (flush) (Thread/sleep 100)))
(dotimes [_ 10] (print ">>" 1 2 3 4 "\n"))
(let [ans (read-line)] (cond (= ans "1") (System/exit 1)  true (System/exit 0) ))
(System/exit (Integer/parseInt (read-line)))





