(ns main4)
(defn dangerous [v1 v2] (quot v1 v2))
(defn foo [array counter]
  (if (zero? counter)
    (dangerous (nth array 0) counter)
    (foo array (dec counter))))
(defn -main []
  (let [array (vec (repeat 1000 0))]
    (println (format "The result is %d" (foo array 6)))))
(-main)

