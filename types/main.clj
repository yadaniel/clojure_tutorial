#!/usr/bin/env clojure

(ns types)

(import java.lang.String)
(import java.lang.Byte)
(import java.lang.Integer)
(import java.lang.Double)
(import java.math.BigDecimal)
(import java.math.BigInteger)
(import java.util.Date)
(import java.io.File)
(import java.nio.file.Path)
(import java.util.Scanner)

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

(.concat "1" (String. "2"))
(.concat (String. "2") "1")
(.concat "1" "2")

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

;; flush is needed, "\n" does not flush
(dotimes [i 10] (do (printf "[%d/%d] = %s\n" i 10 i) (flush) (Thread/sleep 100)))
(dotimes [_ 10] (print ">>" 1 2 3 4 "\n"))
(let [ans (read-line)] (cond (= ans "1") (System/exit 1)  true (System/exit 0) ))
(System/exit (Integer/parseInt (read-line)))

