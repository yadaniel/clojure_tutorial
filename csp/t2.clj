#!/bin/env cljbb.exe
#!/bin/env cljbbw.exe

(require '[clojure.core.async :as a])

;; pr, prn

(println "clojure.core.async num of threads = " )
(println "clojure.core.async enable checks = " )

(loop [n 10]
  (prn n)
  (if (= n 0)
    :stopp
    (recur (dec n))))

(def tf (fn[id]
          (loop [n 5] 
            (Thread/sleep (* 1000 (rand)))
            (println "thread from" id "=>  "(- 5 n))
            (flush)
            (if (= n 0)
              :stop 
              (recur (dec n))))))

(. (Thread. #(tf "java")) start)  ;; java thread
(a/go (a/thread (tf "go")))       ;; go thread
(Thread/sleep 10000)

(def ch (a/chan 10))
(def xs (atom []))

;; put!, take! async
;; put! will not block, even if buffered channel is full
(a/take! ch (fn[x](swap! xs #(conj % x))))      ;; does not block even if channel empty => call fn as callback when ready to read from buffer
(a/put! ch 1 (fn[x](println "put! callback")))  ;; does not block even if channel full => call fn as callback when read to write to buffer
;; (a/put! ch 1)
;; (a/take! ch (fn[x](swap! xs #(conj % x))))

;; (prn (a/<! ch))       ;; block when channel empty
(a/>! ch 1)
(prn (a/<! ch))

;; create channel
(def ch1 (chan 10))
;; start go thread which fills channel
;; (go (loop[n 0] (Thread/sleep 1000) (>! ch1 n) (recur (inc n))))
(go (loop[n 0] (Thread/sleep 1000) (>! ch1 n) (prn "written >!" n) (recur (inc n))))
(go (loop[n 0] (Thread/sleep 1000) (>>! ch1 n) (prn "written >!!" n) (recur (inc n))))
(go (loop[n 0] (Thread/sleep 1000) (put! ch1 n) (prn "written >put!" n) (recur (inc n))))

(Thread/sleep 1000)
(println @xs)

