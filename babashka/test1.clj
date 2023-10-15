#!/usr/bin/env cljbbw.exe
;;#/usr/bin/env cljbb.exe
;;#/mnt/c/bin/cljbb.exe

(defn f [f' v] (f' v))
(prn (f int 1.1))
;; (prn (f Math/ceil 1.1))
(prn (f (fn [v] (Double. (#(format "%.1f" %) v))) 1.1234))
(prn (f #(Math/floor %) 1.1234))
(prn (f #(Math/ceil %) 1.1234))

(def *int* true)
;; (def *int* false)

(defn random-num'
  ([u] (* u (rand)))
  ([l u] (+ l (* (- u l) (rand)))))

(defn random-num
  ([u f] (let [v (random-num' u)] (if *int* (f v) v)))
  ([l u f] (let [v (random-num' l u)] (if *int* (f v) v))))

;; (defn random-num
;;   ([u] (let [v (random-num' u)] (if *int* (Math/ceil v) v)))
;;   ([l u] (let [v (random-num' l u)] (if *int* (Math/ceil v) v))))

(defn random-nums
  ([u n] (take n (repeatedly #(random-num u (fn [x] (Math/ceil x))))))
  ([l u n] (take n (repeatedly #(random-num l u (fn [x] (Math/floor x)))))))

(defn -main [& args]
  (dorun (for [arg args] (prn arg)))
  (let [n (random-nums -10 10 10)]
    (prn "nums => " n)
    (prn "freqs => " (frequencies n)))
  (System/exit 0))

(-main *command-line-args*)

