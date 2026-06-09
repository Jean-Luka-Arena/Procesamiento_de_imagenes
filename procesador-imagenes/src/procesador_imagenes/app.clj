(ns procesador-imagenes.app
  (:require [procesador-imagenes.filters :as filters])
  (:import
    [javax.swing JFrame JPanel JButton JLabel JScrollPane JFileChooser
                 SwingUtilities SwingWorker BorderFactory JOptionPane ImageIcon]
    [javax.swing.border EmptyBorder]
    [java.awt BorderLayout GridLayout Color Cursor Font Dimension]
    [java.awt.image BufferedImage]
    [javax.imageio ImageIO]
    [java.io File]))

;; Estado

(def estado
  (atom {:imagen-original nil
         :imagen-actual   nil
         :archivo-actual  nil
         :pipeline        []}))

;; catalogo de filtros

(def filtros-disponibles
  {:invertir {:nombre "Invertir" :fn filters/invertir}})

(defn nombre-filtro [k]
  (get-in filtros-disponibles [k :nombre] (name k)))

(defn fn-filtro [k]
  (get-in filtros-disponibles [k :fn]))

;; panel de imagen

(defn make-image-panel []
  (let [label (JLabel. "Abrí una imagen desde Archivo → Abrir" JLabel/CENTER)]
    (.setFont label (Font. "SansSerif" Font/PLAIN 14))
    (.setOpaque label true)
    (.setBackground label (Color. 30 30 30))
    (.setForeground label Color/GRAY)
    label))

(defn actualizar-imagen-panel! [^JLabel label img]
  (if img
    (let [panel-w (max 1 (.getWidth label))
          panel-h (max 1 (.getHeight label))
          img-w   (.getWidth img)
          img-h   (.getHeight img)
          scale   (min (/ (double panel-w) img-w)
                       (/ (double panel-h) img-h))
          new-w   (max 1 (int (* img-w scale)))
          new-h   (max 1 (int (* img-h scale)))
          scaled  (.getScaledInstance img new-w new-h BufferedImage/SCALE_SMOOTH)]
      (.setIcon label (ImageIcon. scaled))
      (.setText label ""))
    (do (.setIcon label nil)
        (.setText label "Abrí una imagen desde Archivo → Abrir"))))

;; panel lateral de filtros

(defn refrescar-pipeline-panel! [^JPanel panel pipeline on-remove]
  (.removeAll panel)
  (doseq [[idx k] (map-indexed vector pipeline)]
    (let [filter-row    (JPanel. (BorderLayout.))
          filter-label  (JLabel. (str (inc idx) ". " (nombre-filtro k)))
          remove-button (JButton. "×")]
      (.setOpaque filter-row true)
      (.setBackground filter-row (Color. 55 55 55))
      (.setForeground filter-label Color/WHITE)
      (.setFont filter-label (Font. "SansSerif" Font/PLAIN 12))
      (.setBorder filter-label (EmptyBorder. 2 6 2 2))
      (.setForeground remove-button (Color. 220 80 80))
      (.setContentAreaFilled remove-button false)
      (.setBorderPainted remove-button false)
      (.setFocusPainted remove-button false)
      (.addActionListener remove-button
                          (reify java.awt.event.ActionListener
                            (actionPerformed [_ _] (on-remove idx))))
      (.add filter-row filter-label BorderLayout/CENTER)
      (.add filter-row remove-button BorderLayout/EAST)
      (.add panel filter-row)))
  (.revalidate panel)
  (.repaint panel))

;; archivo

(defn abrir-imagen! [frame]
  (let [fc (JFileChooser.)]
    (.setDialogTitle fc "Abrir imagen")
    (when (= JFileChooser/APPROVE_OPTION (.showOpenDialog fc frame))
      (let [file (.getSelectedFile fc)
            img  (ImageIO/read file)]
        (if img
          (do (swap! estado assoc
                     :imagen-original img
                     :imagen-actual   img
                     :archivo-actual  file
                     :pipeline        [])
              file)
          (JOptionPane/showMessageDialog frame
                                         "No se pudo leer la imagen." "Error" JOptionPane/ERROR_MESSAGE))))))

(defn guardar-como! [frame]
  (let [{:keys [imagen-actual]} @estado]
    (if (nil? imagen-actual)
      (JOptionPane/showMessageDialog frame "No hay imagen para guardar."
                                     "Aviso" JOptionPane/WARNING_MESSAGE)
      (let [fc (JFileChooser.)]
        (.setDialogTitle fc "Guardar como")
        (when (= JFileChooser/APPROVE_OPTION (.showSaveDialog fc frame))
          (let [file (.getSelectedFile fc)
                path (.getAbsolutePath file)
                path (if (.endsWith path ".png") path (str path ".png"))
                file (File. path)]
            (ImageIO/write imagen-actual "png" file)
            (swap! estado assoc :archivo-actual file)))))))

;; aplicar pipeline en background

(defn aplicar-pipeline! [frame image-label set-ui-busy!]
  (let [{:keys [imagen-original pipeline]} @estado]
    (cond
      (nil? imagen-original)
      (JOptionPane/showMessageDialog frame "Primero abrí una imagen."
                                     "Aviso" JOptionPane/WARNING_MESSAGE)
      (empty? pipeline)
      (JOptionPane/showMessageDialog frame "El pipeline está vacío."
                                     "Aviso" JOptionPane/WARNING_MESSAGE)
      :else
      (let [fns    (mapv fn-filtro pipeline)
            worker (proxy [SwingWorker] []
                     (doInBackground []
                       (filters/apply-pipeline imagen-original fns))
                     (done []
                       (let [resultado (try (.get this)
                                            (catch Exception e
                                              (println "Error:" (.getMessage e))
                                              nil))]
                         (when resultado
                           (swap! estado assoc :imagen-actual resultado)
                           (actualizar-imagen-panel! image-label resultado))
                         (set-ui-busy! false))))]
        (set-ui-busy! true)
        (.execute worker)))))

;; ventama principal

(defn crear-ventana []
  (let [frame           (JFrame. "Procesador de Imagenes")
        image-label     (make-image-panel)
        pipeline-panel  (JPanel. (GridLayout. 0 1 2 2))
        scroll-pipeline (JScrollPane. pipeline-panel)
        filtro-keys     (vec (keys filtros-disponibles))
        add-button      (JButton. "Agregar")
        apply-button    (JButton. "Aplicar Pipeline")
        reset-button    (JButton. "Reset")
        status-label    (JLabel. " ")

        refrescar-pipeline!
        (fn refrescar! []
          (refrescar-pipeline-panel!
            pipeline-panel
            (:pipeline @estado)
            (fn [idx]
              (swap! estado update :pipeline
                     (fn [p] (vec (concat (subvec p 0 idx) (subvec p (inc idx))))))
              (refrescar!))))

        set-ui-busy!
        (fn [ocupado?]
          (SwingUtilities/invokeLater
            (fn []
              (.setEnabled apply-button (not ocupado?))
              (.setEnabled add-button   (not ocupado?))
              (.setEnabled reset-button (not ocupado?))
              (.setCursor frame (if ocupado?
                                  (Cursor/getPredefinedCursor Cursor/WAIT_CURSOR)
                                  (Cursor/getDefaultCursor)))
              (.setText status-label (if ocupado? "Procesando…" " ")))))

        menu-bar         (javax.swing.JMenuBar.)
        menu-archivo     (javax.swing.JMenu. "Archivo")
        open-item        (javax.swing.JMenuItem. "Abrir")
        save-as-item     (javax.swing.JMenuItem. "Guardar como…")
        exit-item        (javax.swing.JMenuItem. "Salir")]

    ;; eventos de menu
    (.addActionListener open-item
                        (reify java.awt.event.ActionListener
                          (actionPerformed [_ _]
                            (when (abrir-imagen! frame)
                              (refrescar-pipeline!)
                              (actualizar-imagen-panel! image-label (:imagen-actual @estado))))))

    (.addActionListener save-as-item
                        (reify java.awt.event.ActionListener
                          (actionPerformed [_ _] (guardar-como! frame))))

    (.addActionListener exit-item
                        (reify java.awt.event.ActionListener
                          (actionPerformed [_ _] (System/exit 0))))

    (doto menu-archivo
      (.add open-item)
      (.add save-as-item)
      (.addSeparator)
      (.add exit-item))
    (.add menu-bar menu-archivo)
    (.setJMenuBar frame menu-bar)

    ;; eventos de botones
    (.addActionListener add-button
                        (reify java.awt.event.ActionListener
                          (actionPerformed [_ _]
                            (swap! estado update :pipeline conj (first filtro-keys))
                            (refrescar-pipeline!))))

    (.addActionListener apply-button
                        (reify java.awt.event.ActionListener
                          (actionPerformed [_ _]
                            (aplicar-pipeline! frame image-label set-ui-busy!))))

    (.addActionListener reset-button
                        (reify java.awt.event.ActionListener
                          (actionPerformed [_ _]
                            (swap! estado assoc :pipeline [] :imagen-actual (:imagen-original @estado))
                            (refrescar-pipeline!)
                            (actualizar-imagen-panel! image-label (:imagen-actual @estado)))))

    ;; layout panel lateral
    (.setBackground pipeline-panel (Color. 45 45 45))
    (.setPreferredSize scroll-pipeline (Dimension. 210 0))
    (.setBorder scroll-pipeline
                (BorderFactory/createTitledBorder
                  (BorderFactory/createLineBorder (Color. 80 80 80)) "Filtros"))

    (let [side-panel   (JPanel. (BorderLayout. 4 4))
          bottom-panel (JPanel. (GridLayout. 4 1 4 4))]
      (.setBackground side-panel (Color. 40 40 40))
      (.setBackground bottom-panel (Color. 40 40 40))
      (.setBorder side-panel (EmptyBorder. 8 8 8 8))

      (doseq [c [add-button apply-button reset-button]]
        (.setBackground c (Color. 60 60 60))
        (.setForeground c Color/WHITE))
      (.setForeground status-label (Color. 200 180 0))
      (.setHorizontalAlignment status-label JLabel/CENTER)

      (doto bottom-panel
        (.add add-button)
        (.add apply-button)
        (.add reset-button)
        (.add status-label))

      (doto side-panel
        (.add scroll-pipeline BorderLayout/CENTER)
        (.add bottom-panel BorderLayout/SOUTH))

      (let [center-scroll (JScrollPane. image-label)]
        (.setBackground (.getViewport center-scroll) (Color. 30 30 30))
        (doto (.getContentPane frame)
          (.setBackground (Color. 30 30 30))
          (.add center-scroll BorderLayout/CENTER)
          (.add side-panel BorderLayout/EAST))))

    (doto frame
      (.setSize 1000 650)
      (.setMinimumSize (Dimension. 700 450))
      (.setLocationRelativeTo nil)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))

    frame))

;; punto de entrada

(defn iniciar! []
  (SwingUtilities/invokeLater
    (fn [] (crear-ventana))))