(defproject procesador-imagenes "0.1.0-SNAPSHOT"
  :description "Procesador de imágenes con filtros compuestos - TP2"
  :dependencies [[org.clojure/clojure "1.12.2"]]
  :main procesador-imagenes.core
  :aot [procesador-imagenes.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})