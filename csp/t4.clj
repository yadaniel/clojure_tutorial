#!/bin/env cljbb.exe
#!/bin/env cljbbw.exe

(require '[clojure.core.async :as a])

;; create channel
(def ch1 (a/chan 10))

(def DELAY 5000)

(a/go (let [[x y] (a/alts! [ch1 (a/timeout 10000)])]
        (if (not (nil? x))
          (println "reading from channel =" x)
          (println "timeout"))))

;; (a/go (let [[x y] (a/alts! [ch1 (a/timeout 10000)])]
;;         (if (= y (a/timeout 0))
;;           (println "timeout"))
;;           (println "reading from channel =" x)))

(Thread/sleep 5000)
(Thread/sleep 11000)
(a/>!! ch1 1234)

(loop []
  (Thread/sleep 100)
  (recur))

