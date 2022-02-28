#!/usr/bin/bash clojure

(ns main-app)
(require 'clojure.repl)
(require 'clojure.repl 'clojure.string)
(require 'clojure.repl 'clojure.string 'clojure.data)
(require 'clojure.repl 'clojure.string 'clojure.data 'clojure.pprint)
(require 'clojure.repl 'clojure.string 'clojure.data 'clojure.pprint 'clojure.set)
(require 'clojure.repl 'clojure.string 'clojure.data 'clojure.pprint 'clojure.set 'clojure.reflect)

;; introduce names
;; no value is bound
(def v)
(defn f[])
(defrecord r[])

(println "\nmain-app")
(clojure.repl/dir main-app)


(println "\nclojure.repl")
(clojure.repl/dir clojure.repl)

(clojure.repl/source clojure.string/reverse)
(println "ns-pubics =>" (ns-publics 'main-app))
