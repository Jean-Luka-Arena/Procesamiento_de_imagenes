(ns procesador-imagenes.filters)

(defn- clamp [v] (max 0 (min 255 v)))

(defn- rgb->components [rgb]
  {:a (bit-and (bit-shift-right rgb 24) 0xFF)
   :r (bit-and (bit-shift-right rgb 16) 0xFF)
   :g (bit-and (bit-shift-right rgb 8)  0xFF)
   :b (bit-and rgb 0xFF)})

(defn- components->rgb [{:keys [a r g b]}]
  (unchecked-int (bit-or (bit-shift-left (long a) 24)
                         (bit-shift-left (long r) 16)
                         (bit-shift-left (long g) 8)
                         (long b))))

(defn invertir [img]
  (let [w (.getWidth img)
        h (.getHeight img)
        result (java.awt.image.BufferedImage. w h (.getType img))]
    (doseq [y (range h)
            x (range w)]
      (let [{:keys [a r g b]} (rgb->components (.getRGB img x y))
            new-rgb (components->rgb {:a a
                                      :r (- 255 r)
                                      :g (- 255 g)
                                      :b (- 255 b)})]
        (.setRGB result x y new-rgb)))
    result))

(defn apply-pipeline [img filtros]
  (reduce (fn [imagen filtro] (filtro imagen)) img filtros))