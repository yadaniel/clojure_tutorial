#!/usr/bin/env cljbbw.exe
#!/usr/bin/env cljbb.exe
#!/mnt/c/bin/cljbb.exe


(def cnt (atom 0))
(defn thread [] (dotimes [_ 10] (print ".") (flush) (Thread/sleep (* (rand) 100) (swap! cnt inc))))
(dotimes [_ 10] (.start (Thread. thread)) (Thread/sleep 10))
(Thread/sleep 1000)
(println "cnt=" @cnt)

(defn -main []
  (println "babashka main")
  (def a (atom 0))
  (defn thread [id] (println) (dotimes [_ 10] (printf "<%s>," id) (flush) (Thread/sleep (* (rand) 100) (swap! a inc))))
  (def threads (for [id (range 10)] (Thread. (fn[] (thread id)))))
  ;; lazy sequence
  ;; (map (fn[t](.start t)) threads) 
  ;; (map (fn[t](.join t)) threads)
  ;; (dorun (map (fn[t](.start t)) threads))
  ;; (dorun (map (fn[t](.join t)) threads))
  ;; lazy sequence
  ;; (for [t threads] (doto t .start .join))
  (def _ (for [t threads] (doto t .start .join)))
  (println _)   ;; this println forces lazy sequence to evaluate
  ;; (dorun (for [t threads] (doto t .start .join)))
  ;; (dotimes [_ 10] (.start (Thread. thread)) (Thread/sleep 100))
  ;; (dotimes [_ 10] (.start (Thread. thread)) (Thread/sleep 10))
  (Thread/sleep 1000)
  (println @a)
  @a)

;; call main
(-main)
(printf "a=%s\n" @a)

;; lazy sequence forced to evaluate, after binding and side-effect with println
(def xs (for[i (range 10)]i))
(println xs)

;; lazy sequence is not forced to evaluate by side-effect with println
(for [i (range 5)]
  (println i))

;; force it to run
(dorun (for [i (range 5)]
  (println i)))

