#!/usr/bin/env clojure

;; thread
(def ct (Thread/currentThread))
(println "thread id =" (.getId ct))
(println "thread name (before) =" (.getName ct))
(println "thread set name" (.setName ct "script"))
(println "thread name (after) =" (.getName ct))
(println "thread priority =" (.getPriority ct))
(println "thread isInterrupted (before) =" (.isInterrupted ct))
(println "thread interrupt =" (.interrupt ct))
(println "thread isInterrupted (after) =" (.isInterrupted ct))
(println "thread isAlive =" (.isAlive ct))
(println "thread isDaemon =" (.isDaemon ct))
(println "thread/stack =" (Thread/dumpStack))
(println "thread/active count =" (Thread/activeCount))
(println "thread/holds lock =" (Thread/holdsLock 1))

;; context loader
(def cl (.getContextClassLoader ct))
(println "context loader" cl)

(require '[clojure.java.io :as io])
(println "io resource of main.class =>" (io/resource "clojure/main.class"))

(require 'clojure.pprint)
(refer 'clojure.pprint)
;; (-> (Thread/currentThread) (.getContextClassLoader) (.getURLs) (seq) (pprint))
;; (-> (Thread/currentThread) (.getContextClassLoader) (.getURLs) (seq) (pprint))

(import java.lang.ClassLoader)
(def c(ClassLoader/getSystemClassLoader))

(import java.net.URL)
(import java.net.URLClassLoader)



