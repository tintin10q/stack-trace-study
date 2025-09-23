(ns main)
(defn dangerous [array index] (nth array (+ index 2)))
(defn foo  [array index] (dangerous array index))
(defn foo1 [array index] (foo array (* index 3)))
(defn foo2 [array index] (foo1 array (+ index 137)))
(defn foo3 [array index] (foo2 array (dec index)))
(defn foo4 [array index] (foo3 array (* index 137)))
(defn foo5 [array index] (foo4 array (+ index 20)))
(defn foo6 [array index] (foo5 array (quot index 3)))
(defn -main []
  (let [array (vec (repeat 1000 0))]
    (println (foo6 array 50))))
(-main)

