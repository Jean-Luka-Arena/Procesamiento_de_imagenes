(ns procesador-imagenes.app
    (:require [procesador-imagenes.filters :as filters])
    (:import
      [javax.swing JFrame JPanel JButton JLabel JScrollPane JFileChooser
       SwingUtilities SwingWorker BorderFactory JOptionPane ImageIcon JComboBox]
      [javax.swing.border EmptyBorder]
      [java.awt BorderLayout GridLayout Color Cursor Font Dimension Graphics2D RenderingHints]
      [java.awt.image BufferedImage]
      [javax.imageio ImageIO]
      [java.io File]))

(def estado
  (atom {:imagen-original nil
         :imagen-actual   nil
         :archivo-actual  nil
         :pipeline        []}))

(def filtros-disponibles
  {:invertir   {:nombre "Invertir"   :fn filters/invertir}
   :desaturar  {:nombre "Desaturar"  :fn filters/desaturar}
   :difuminado {:nombre "Difuminado" :fn filters/difuminado}
   :brillo  {:nombre "Brillo"  :fn filters/brillo}
   :saturar {:nombre "Saturar" :fn filters/saturar}})

(defn nombre-filtro [k]
      (get-in filtros-disponibles [k :nombre] (name k)))

(defn fn-filtro [k]
      (get-in filtros-disponibles [k :fn]))

(defn make-image-panel []
      (let [img-atom (atom nil)
            panel    (proxy [JPanel] []
                            (paintComponent [^Graphics2D g]
                                            (proxy-super paintComponent g)
                                            (when-let [img @img-atom]
                                                      (let [pw    (.getWidth this)
                                                            ph    (.getHeight this)
                                                            iw    (.getWidth img)
                                                            ih    (.getHeight img)
                                                            scale (min (/ (double pw) iw) (/ (double ph) ih))
                                                            nw    (int (* iw scale))
                                                            nh    (int (* ih scale))
                                                            ox    (int (/ (- pw nw) 2))
                                                            oy    (int (/ (- ph nh) 2))]
                                                           (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                                                                              RenderingHints/VALUE_INTERPOLATION_BILINEAR)
                                                           (.drawImage g img ox oy nw nh nil)))))]
           (.setBackground panel (Color. 30 30 30))
           {:panel panel :img-atom img-atom}))

(defn actualizar-imagen-panel! [{:keys [panel img-atom]} img]
      (reset! img-atom img)
      (.repaint panel))

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

(defn guardar! [frame]
      (let [{:keys [imagen-actual archivo-actual]} @estado]
           (cond
             (nil? imagen-actual)
             (JOptionPane/showMessageDialog frame "No hay imagen para guardar."
                                            "Aviso" JOptionPane/WARNING_MESSAGE)
             (nil? archivo-actual)
             (JOptionPane/showMessageDialog frame "No hay archivo asociado. Usá Guardar como."
                                            "Aviso" JOptionPane/WARNING_MESSAGE)
             :else
             (ImageIO/write imagen-actual "png" archivo-actual))))

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

(defn aplicar-pipeline! [frame image-panel set-ui-busy!]
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
                                                  (actualizar-imagen-panel! image-panel resultado))
                                            (set-ui-busy! false))))]
                  (set-ui-busy! true)
                  (.execute worker)))))

(defn crear-ventana []
      (let [frame           (JFrame. "Procesador de Imagenes")
            {:keys [panel img-atom] :as image-panel} (make-image-panel)
            pipeline-panel  (JPanel. (GridLayout. 0 1 2 2))
            scroll-pipeline (JScrollPane. pipeline-panel)
            filtro-keys     (vec (keys filtros-disponibles))
            filtro-nombres  (into-array String (map nombre-filtro filtro-keys))
            selector        (JComboBox. filtro-nombres)
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

            menu-bar     (javax.swing.JMenuBar.)
            menu-archivo (javax.swing.JMenu. "Archivo")
            open-item    (javax.swing.JMenuItem. "Abrir")
            save-item    (javax.swing.JMenuItem. "Guardar")
            save-as-item (javax.swing.JMenuItem. "Guardar como…")
            exit-item    (javax.swing.JMenuItem. "Salir")]

           (.addActionListener open-item
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _]
                                                       (when (abrir-imagen! frame)
                                                             (refrescar-pipeline!)
                                                             (actualizar-imagen-panel! image-panel (:imagen-actual @estado))))))

           (.addActionListener save-item
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _] (guardar! frame))))

           (.addActionListener save-as-item
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _] (guardar-como! frame))))

           (.addActionListener exit-item
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _] (System/exit 0))))

           (doto menu-archivo
                 (.add open-item)
                 (.add save-item)
                 (.add save-as-item)
                 (.addSeparator)
                 (.add exit-item))
           (.add menu-bar menu-archivo)
           (.setJMenuBar frame menu-bar)

           (.addActionListener add-button
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _]
                                                       (let [idx (.getSelectedIndex selector)
                                                             k   (nth filtro-keys idx)]
                                                            (swap! estado update :pipeline conj k)
                                                            (refrescar-pipeline!)))))

           (.addActionListener apply-button
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _]
                                                       (aplicar-pipeline! frame image-panel set-ui-busy!))))

           (.addActionListener reset-button
                               (reify java.awt.event.ActionListener
                                      (actionPerformed [_ _]
                                                       (swap! estado assoc :pipeline [] :imagen-actual (:imagen-original @estado))
                                                       (refrescar-pipeline!)
                                                       (actualizar-imagen-panel! image-panel (:imagen-actual @estado)))))

           (.setBackground pipeline-panel (Color. 45 45 45))
           (.setPreferredSize scroll-pipeline (Dimension. 210 0))
           (.setBorder scroll-pipeline
                       (BorderFactory/createTitledBorder
                         (BorderFactory/createLineBorder (Color. 80 80 80)) "Filtros"))

           (let [side-panel   (JPanel. (BorderLayout. 4 4))
                 bottom-panel (JPanel. (GridLayout. 5 1 4 4))]
                (.setBackground side-panel (Color. 40 40 40))
                (.setBackground bottom-panel (Color. 40 40 40))
                (.setBorder side-panel (EmptyBorder. 8 8 8 8))

                (.setBackground selector (Color. 60 60 60))
                (.setForeground selector Color/WHITE)
                (.setOpaque selector true)

                (doseq [c [add-button apply-button reset-button]]
                       (.setBackground c (Color. 60 60 60))
                       (.setForeground c Color/WHITE)
                       (.setOpaque c true)
                       (.setBorderPainted c false))

                (.setForeground status-label (Color. 200 180 0))
                (.setHorizontalAlignment status-label JLabel/CENTER)

                (doto bottom-panel
                      (.add selector)
                      (.add add-button)
                      (.add apply-button)
                      (.add reset-button)
                      (.add status-label))

                (doto side-panel
                      (.add scroll-pipeline BorderLayout/CENTER)
                      (.add bottom-panel BorderLayout/SOUTH))

                (doto (.getContentPane frame)
                      (.setBackground (Color. 30 30 30))
                      (.add panel BorderLayout/CENTER)
                      (.add side-panel BorderLayout/EAST)))

           (doto frame
                 (.setSize 1000 650)
                 (.setMinimumSize (Dimension. 700 450))
                 (.setLocationRelativeTo nil)
                 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                 (.setVisible true))

           frame))

(defn iniciar! []
      (javax.swing.UIManager/setLookAndFeel "javax.swing.plaf.metal.MetalLookAndFeel")
      (SwingUtilities/invokeLater
        (fn [] (crear-ventana))))