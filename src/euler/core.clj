(ns euler.core)

(defn -main [& args]
  (try
    (let [prob (load-file
                (str "src/euler/euler_" (first args) ".clj"))]
      (prob -main))

    (catch Exception e (do
                         (println "Invalid/ Unsolved Problem")
                         (println "Usage: lein run problem-number")))))
