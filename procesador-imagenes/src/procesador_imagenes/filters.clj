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

(defn- apply-filter-parallel [img pixel-fn]
       (let [w      (.getWidth img)
             h      (.getHeight img)
             result (java.awt.image.BufferedImage. w h (.getType img))
             rows   (pmap (fn [y]
                              (mapv (fn [x] [x y (pixel-fn img x y)]) (range w)))
                          (range h))]
            (doseq [row rows [x y rgb] row]
                   (.setRGB result x y rgb))
            result))

(defn invertir [img]
      (apply-filter-parallel img
                             (fn [src x y]
                                 (let [{:keys [a r g b]} (rgb->components (.getRGB src x y))]
                                      (components->rgb {:a a :r (- 255 r) :g (- 255 g) :b (- 255 b)})))))

(defn desaturar [img]
      (apply-filter-parallel img
                             (fn [src x y]
                                 (let [{:keys [a r g b]} (rgb->components (.getRGB src x y))
                                       gris (clamp (int (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))]
                                      (components->rgb {:a a :r gris :g gris :b gris})))))

(defn brillo [img]
      (apply-filter-parallel img
                             (fn [src x y]
                                 (let [{:keys [a r g b]} (rgb->components (.getRGB src x y))]
                                      (components->rgb {:a a
                                                        :r (clamp (+ r 60))
                                                        :g (clamp (+ g 60))
                                                        :b (clamp (+ b 60))})))))

(defn saturar [img]
      (apply-filter-parallel img
                             (fn [src x y]
                                 (let [{:keys [a r g b]} (rgb->components (.getRGB src x y))
                                       gris (int (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))]
                                      (components->rgb {:a a
                                                        :r (clamp (int (+ gris (* 1.8 (- r gris)))))
                                                        :g (clamp (int (+ gris (* 1.8 (- g gris)))))
                                                        :b (clamp (int (+ gris (* 1.8 (- b gris)))))})))))

(defn difuminado [img]
      (let [w (.getWidth img)
            h (.getHeight img)]
           (apply-filter-parallel img
                                  (fn [src x y]
                                      (let [vecinos (for [dy [-1 0 1] dx [-1 0 1]
                                                          :let [nx (max 0 (min (dec w) (+ x dx)))
                                                                ny (max 0 (min (dec h) (+ y dy)))]
                                                          ]
                                                         (rgb->components (.getRGB src nx ny)))
                                            n       (count vecinos)
                                            avg     (fn [k] (clamp (int (/ (reduce + (map k vecinos)) n))))]
                                           (components->rgb {:a (:a (rgb->components (.getRGB src x y)))
                                                             :r (avg :r) :g (avg :g) :b (avg :b)}))))))

(defn apply-pipeline [img filtros]
      (reduce (fn [imagen filtro] (filtro imagen)) img filtros))