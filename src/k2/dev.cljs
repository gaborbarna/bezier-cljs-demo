(ns k2.dev
  (:require [k2.core :as core]))

(defonce animation-started (atom false))

(when-not @animation-started
  (let [canvas (.getElementById js/document "shape-canvas")]
    (core/start-loop canvas nil)
    (reset! animation-started true)))

(defn on-js-load [])
