#!/usr/bin/env clojure

(println "clojure-version =>" (clojure-version))

(dotimes [i 10] (do (java.lang.Thread/sleep 100) (println "ms =>" (java.lang.System/currentTimeMillis))))

;; run gc
(java.lang.System/gc)

;; CLASSPATH=. clojure
(println "classpath =>" (System/getProperty "java.class.path"))

(def env (java.lang.System/getenv))
(println "env keys =>" (keys env))
(println "env PATH =>" (get env "PATH"))
(def path_items(clojure.string/split (get env "PATH") #":"))
(println path_items)
(println (count path_items))
(println (clojure.string/join \newline path_items))

(def c(java.lang.System/console))
(def line (.readLine c))
(def pass (.readPassword c))
(.flush c)
(println line " with password =>" pass)

;; newline does not flush the output => use println or flush
(def r (.reader c))
(def w (.writer c))
(.read r)
(.print w \f)
(.print w "oo")
(.print w \newline)
(.flush w)
(.println w "foo\n")

(println "all true =>" (and
(string? "")
(number? 1)
(number? 1.0)
(number? (/ 1 10))
(number? 1N)
(number? 1M)
(integer? 1)
(float? 1.0)
(rational? 1)
(rational? 1N)
(rational? 1M)
(rational? (/ 1 10))
(boolean? false)
(boolean? true)
(symbol? 'foo)
(keyword? :foo)
(fn? map)
(map? {})
(set? #{})
(vector? [])
(list? '())
))

(clojure.core/every? odd? [])
(clojure.core/every? odd? [])
(clojure.core/some? [])
;; any? returns always true
(clojure.core/any? [])

;; exit
(java.lang.System/exit 1)


