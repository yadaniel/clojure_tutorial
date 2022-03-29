#!/usr/bin/env clojure

(ns test
    (:gen-class))

(defn main[] (println "entry point"))

;; :foo => keyword
;; 'foo => symbol
;; "foo" => string

(import java.io.File)
(require '[clojure.java.io :as io])

(defn ask-file-try[] (do
  (print "filename: ")
  (flush)
  (def infile (read-line))
  (if (.exists (io/file infile)) '(true infile) '(false ""))))

(defn ask-file-existing[] (do
  (print "filename: ")
  (flush)
  (def infile (read-line))
  (if (.exists (io/file infile)) infile nil)))

(defn ^String ask-file[^String msg] (do
  (print msg "filename: ")
  (flush)
  (read-line)))

(defn ^String read-file[^String infile] 
  (if infile (slurp infile) nil))

(defn write-file[^String outfile ^String content] 
  (spit outfile content))

;; (def lines (clojure.string/split-lines (read-file (ask-file-existing))))
;; (println "lines =>" (count lines))

;; (def text (read-file (ask-file "input")))
;; (write-file (ask-file "output") text)

;; import used for java interop
(import java.lang.Character)
(import java.lang.Byte)
(import java.lang.Short)
(import java.lang.Integer)
(import java.lang.Long)
(import java.lang.Float)
(import java.lang.Double)
(import java.lang.String)

(= (Character. \1) (char \1))
(= (Byte. "1") (byte 1))
(= (Short. "1") (short 1))
(= (Integer. "1") (int 1))
(= (Long. "1") (long 1))
(= (Float. "1.0") (float 1.0))
(= (Float. "1.0f") (float 1.0))   ;; 1.0f not in clojure
(= (Double. "1.0") (double 1.0))
(= (Double. "1.0d") (double 1.0))   ;; 1.0d not in clojure
(= (String.) (str))
(= (String. "") (str ""))

(defn ^char f[] \A)
(defn ^byte b[] 1)
(defn ^short s[] 1)
(defn ^int i[] 1)
(defn ^long l[] 1)
(defn ^float f[] 1.0)
(defn ^double d[] 1.0)

;; general form
(defn ^double fun "" {} [] (comment expression not required here, nil))

;; loading clojure libraris
(require 'clojure.pprint)
(require 'clojure.reflect)
(require 'clojure.string)
(require 'clojure.data)
;; (dir clojure.data)

