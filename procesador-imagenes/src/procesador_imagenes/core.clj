(ns procesador-imagenes.core
  (:require [procesador-imagenes.app :as app])
  (:gen-class))

(defn -main [& _args]
  (app/iniciar!))