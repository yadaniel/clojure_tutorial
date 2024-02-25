#!/usr/bin/env cljbbw.exe

(require '[clojure.string :as string])

(defn pl1[s f p w]
  (let [t (string/upper-case (str s f p w))]
    (cond 
      (= t "S1F1P1L") :a
      (= t "S1F1P1H") :a

      (= t "S1F1P2L") :a
      (= t "S1F1P2H") :b

      (= t "S1F2P1L") :a
      (= t "S1F2P1H") :b

      (= t "S1F2P2L") :b
      (= t "S1F2P2H") :c

      (= t "S2F1P1L") :b
      (= t "S2F1P1H") :c

      (= t "S2F1P2L") :c
      (= t "S2F1P2H") :d

      (= t "S2F2P1L") :c
      (= t "S2F2P1H") :d

      (= t "S2F2P2L") :d
      (= t "S2F2P2H") :e

      true :undefined
      )))

(defn pl2[s f p w]
  (let [t (string/upper-case (str s f p w))]
    (condp (fn[l r] (= l r)) t

      "S1F1P1L" :a
      "S1F1P1H" :a

      "S1F1P2L" :a
      "S1F1P2H" :b

      "S1F2P1L" :a
      "S1F2P1H" :b

      "S1F2P2L" :b
      "S1F2P2H" :c

      "S2F1P1L" :b
      "S2F1P1H" :c

      "S2F1P2L" :c
      "S2F1P2H" :d

      "S2F2P1L" :c
      "S2F2P1H" :d

      "S2F2P2L" :d
      "S2F2P2H" :e

      :undefined
      )))

(defn pl3[s f p w]
  (let [s' (if (= (string/upper-case s) "S2") 4 0)
        f' (if (= (string/upper-case f) "F2") 2 0)
        p' (if (= (string/upper-case p) "P2") 1 0)
        w' (if (= (string/upper-case w) "H") true false)
        t (+ s' f' p')
        tbl {
             0 0,   ;; a
             1 1,   ;; b
             2 1,   ;; b
             3 2,   ;; c
             4 2,   ;; c
             5 3,   ;; d
             6 3,   ;; d
             7 4,   ;; e
             }
        v (tbl t)
        v' (max (if w' (dec v) v) 0)
        ]

        ({
         0 :a,
         1 :b,
         2 :c,
         3 :d,
         4 :e,
         } v')

    ))

;; (def s (read))
;; (def f (read))
;; (def p (read))
;; (def w (read))

(def params [
  ["s1" "f1" "p1" "l"]
  ["s1" "f1" "p1" "h"]
  ["s1" "f1" "p2" "l"]
  ["s1" "f1" "p2" "h"]

  ["s1" "f2" "p1" "l"]
  ["s1" "f2" "p1" "h"]
  ["s1" "f2" "p2" "l"]
  ["s1" "f2" "p2" "h"]

  ["s2" "f1" "p1" "l"]
  ["s2" "f1" "p1" "h"]
  ["s2" "f1" "p2" "l"]
  ["s2" "f1" "p2" "h"]

  ["s2" "f2" "p1" "l"]
  ["s2" "f2" "p1" "h"]
  ["s2" "f2" "p2" "l"]
  ["s2" "f2" "p2" "h"]
  ])

(def r (for [param params] (apply pl1 param)))
(println r)

(def r (for [param params] (apply pl2 param)))
(println r)

(def r (for [param params] (apply pl3 param)))
(println r)

;; (System/exit 0)

(require '[clojure.string :as string])

(def s-pattern #"[s|S][1|2]")
(def f-pattern #"[f|F][1|2]")
(def p-pattern #"[p|P][1|2]")
(def w-pattern #"(?i)[l|h]")

(defn s-find-consume[v] 
  (let [s-value (re-find s-pattern v)
        v-rest (string/replace v (or s-value "") "")]
        [s-value, v-rest]))

(defn f-find-consume[v] 
  (let [f-value (re-find f-pattern v)
        v-rest (string/replace v (or f-value "") "")]
        [f-value, v-rest]))

(defn p-find-consume[v] 
  (let [p-value (re-find p-pattern v)
        v-rest (string/replace v (or p-value "") "")]
        [p-value, v-rest]))

(defn w-find-consume[v] 
  (let [w-value (re-find w-pattern v)
        v-rest (string/replace v (or w-value "") "")]
        [w-value, v-rest]))

(defn sfp-find-consume[v]
    (let [[s-value v1] (s-find-consume (string/replace v " " ""))
          [f-value v2] (f-find-consume v1)
          [p-value v3] (p-find-consume v2)
          [w-value v4] (w-find-consume v3)
          m {:S s-value, :F f-value, :P p-value, :W w-value}
          valid (not (or 
                        (nil? s-value) 
                        (nil? f-value) 
                        (nil? p-value) 
                        (nil? w-value)
                        (not (empty? v4))))]
          (if valid m nil)))

(println (sfp-find-consume "s1f1p1l"))
(println (sfp-find-consume "s1f1f1l"))
(println (sfp-find-consume "s1p1f1l"))

(println (sfp-find-consume "s1 p1 f1 l"))
(println (sfp-find-consume "s1 l f1 p1"))

;; (println (pl1 "s2" "f1" "p2" "h"))
;; (println (pl2 "s2" "f1" "p2" "h"))
;; (println (pl3 "s2" "f1" "p2" "h"))

;; (println (pl1 s f p w))
;; (println (pl2 s f p w))

(def tests [
  {:in ["s1" "f1" "p1" "l"], :out :a}
  {:in ["s1" "f1" "p1" "h"], :out :a}
  {:in ["s1" "f1" "p2" "l"], :out :a}
  {:in ["s1" "f1" "p2" "h"], :out :b}

  {:in ["s1" "f2" "p1" "l"], :out :a}
  {:in ["s1" "f2" "p1" "h"], :out :b}
  {:in ["s1" "f2" "p2" "l"], :out :b}
  {:in ["s1" "f2" "p2" "h"], :out :c}

  {:in ["s2" "f1" "p1" "l"], :out :b}
  {:in ["s2" "f1" "p1" "h"], :out :c}
  {:in ["s2" "f1" "p2" "l"], :out :c}
  {:in ["s2" "f1" "p2" "h"], :out :d}

  {:in ["s2" "f2" "p1" "l"], :out :c}
  {:in ["s2" "f2" "p1" "h"], :out :d}
  {:in ["s2" "f2" "p2" "l"], :out :d}
  {:in ["s2" "f2" "p2" "h"], :out :e}
  ])

(def tests-in (for [test tests] (:in test)))
(def tests-out (for [test tests] (:out test)))

(println tests-out)
(println tests-in)


