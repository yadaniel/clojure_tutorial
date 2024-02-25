#!/usr/bin/env cljbbw.exe
;;#/usr/bin/env cljbb.exe
;;#/mnt/c/bin/cljbb.exe

(require '[clojure.tools.cli :as cli])

(def cli-options
  [["-n" "--name NAME" "Your name"]
   ["-a" "--age AGE" "Your age" :parse-fn #(Integer. %)]])

;; (println (cli/parse-opts ["-n FOO1"] cli-options))
;; (read)
;; (println (cli/parse-opts ["-n" "FOO2"] cli-options))
;; (read)
;; (println (cli/parse-opts ["-a" "77"] cli-options))
;; (read)
;; (println (cli/parse-opts ["-n" "FOOBAR" "-a" "71"] cli-options))
;; (read)
;; (println (cli/parse-opts *command-line-args* cli-options))
;; (read)
;; (System/exit 0)

;; (defn -main [& [args]]
;;   (let [opts (cli/parse-opts args cli-options)]
;;   ;; (let [opts (cli/parse-opts (first args) cli-options)]
;;   ;; (let [opts (cli/parse-opts ["-n" "foo" "-a" "1"] cli-options)]
;;     (println "Options:" (:options opts))
;;     (println "Arguments:" (:arguments opts))
;;     (println "Summary:" (:summary opts))
;;     (when (:help opts)
;;       (println (:usage opts)))))

(defn -main [& args]
  (let [opts (cli/parse-opts (flatten args) cli-options)]
    (println "Options:" (:options opts))
    (println "Arguments:" (:arguments opts))
    (println "Summary:" (:summary opts))
    (when (:help opts)
      (println (:usage opts)))))


(-main *command-line-args*)
(System/exit 0)


