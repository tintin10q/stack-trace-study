(ns main3
  (:gen-class))

(defn dangerous [array index]
  (nth array index)) ;; IndexOutOfBoundsException

(defn foo [array counter]
  (if (zero? counter)
    (dangerous array (+ counter 9137))
    (recur array (dec counter))))

(defn -main [& _]
  (let [arr (vec (repeat 1000 0))
        result (foo arr 900)]
    (println "The result is" result)))
(-main)
