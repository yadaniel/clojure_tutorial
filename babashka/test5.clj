#!/usr/bin/env cljbbw.exe
;;#/usr/bin/env cljbb.exe
;;#/mnt/c/bin/cljbb.exe

(require '[clojure.tools.cli :as cli])
(require '[babashka.cli :as cli_])
(require '[babashka.fs :as fs])
(require '[clojure.core.async :as async])

;; unbounded channel
(def c1 (async/chan))

;; bounded channel
(def c2 (async/chan 10))

;; dropping buffer channel
(def c3 (async/chan (async/dropping-buffer 10)))

;; sliding buffer channel
(def c4 (async/chan (async/sliding-buffer 10)))

(doall (for[c [c1 c2 c3 c4]] (println (type c))))

(System/exit 0)

