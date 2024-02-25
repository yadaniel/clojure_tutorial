#!/usr/bin/env cljbbw.exe
;;#/usr/bin/env cljbb.exe
;;#/mnt/c/bin/cljbb.exe

(require '[clojure.tools.cli :as cli])
(require '[babashka.cli :as cli_])
(require '[babashka.fs :as fs])

(println (fs/which "bash"))
(println (fs/which-all "bash"))
(println (fs/list-dir "."))
(println (fs/cwd))
(println (fs/size "./test4.clj"))
(println (fs/windows?))
;; (println (fs/read-attributes "./test4.clj"))
(println "glob" (fs/glob (fs/cwd) "t*4*"))

(dorun (for[line (fs/read-all-lines "./test4.clj")] (println line)))

(defn -main [& args]
  (if (fs/exists? ".") 
    (println ". exists")
    (println ". does not exists"))
  (println "in main"))

(-main *command-line-args*)
(System/exit 0)


