#!/bin/env cljbb.exe
#!/bin/env cljbbw.exe

(use 'clojure.core.async)

(def ch (chan 10))

(defn pf[]
  (loop [n 1]
    (Thread/sleep (* 1000 (rand)))  ;; sleep upto 1 second
    (let [w (rand)]                 ;; produce w
      (printf ">>> producer awaiken [%s] ... puts %s\n" n w)
      (flush)                       ;; tell about it
      (>! ch w))                    ;; put it on then channel and block if full
    (recur (inc n))))

(defn cf[]
  (loop [n 1]
    (Thread/sleep (* 2000 (rand)))  ;; sleep upto 2 second
    (let [w (<! ch)]                ;; get w
      (printf "consumer awaiken [%s] ... gets %s\n" n w)
      (flush))                      ;; tell about it
    (recur (inc n))))

;; ;; create producer
;; (def p (Thread. pf))
;; (. p start)

;; ;; create consumer
;; (def c (Thread. cf))
;; (. c start)

;; (= (char 10) \newline)
;; (= 10 (int \newline))

;; main thread
(loop []
  (Thread/sleep 500)
  (println "main ...")
  (let [c (.read *in*)]
    (printf "<%s> == <%s>\n" c (int \newline))
    ;; (when (= c (int \newline)) (System/exit 0)))
    (when false (System/exit 0)))
  (recur))


