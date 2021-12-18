(ns myLib1)

(defn swap_default [x y] (vector y x))
(defn ^:publica swapa [x y] (vector y x))
(defn ^:public swap [x y] (vector y x))
(defn ^:private swap' [x y] [y x])


