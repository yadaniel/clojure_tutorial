#!/bin/env cljbb.exe
#!/bin/env cljbbw.exe

(require '[clojure.core.async :as a])

;; create channel
(def ch1 (a/chan 10))

(def DELAY 5000)

;; start go thread which fills channel
;; (a/go (loop[n 0] (Thread/sleep DELAY) (a/>! ch1 n) (prn "written >!" n) (recur (inc n))))
;; (a/go (loop[n 0] (Thread/sleep DELAY) (a/>>! ch1 n) (prn "written >!!" n) (recur (inc n))))
;; (a/go (loop[n 0] (Thread/sleep DELAY) (a/put! ch1 n) (prn "written >put!" n) (recur (inc n))))
(a/go (loop[n 0] (Thread/sleep DELAY) (a/put! ch1 n (fn[x](prn "on buffer"))) (prn "written >put!" n) (recur (inc n))))

(loop []
  (Thread/sleep 100)
  (recur))

