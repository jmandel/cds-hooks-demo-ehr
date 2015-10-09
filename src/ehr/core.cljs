(ns ehr.core
  (:require
   [ehr.actions :as actions]
   [ehr.util :as util :refer [register app-state reducers dispatch next-tick]]
   [ehr.rxnorm :as rxnorm]
   [ehr.rxpad :as rxpad]
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)
(declare on-js-reload)

  (js/console.log "HASH" (js/location.hash.slice 1)) 
(defn home-page []
  (let [state @app-state]
    [:div [:h2 "Demo EHR"]
     [rxpad/drug-selector (state :drug-store)]
     [:div [:a {:href "#/about"} "go to about page"]]]))


(defn on-js-reload []
    (next-tick util/on-hash-change)
    (swap! app-state update-in [:__figwheel_counter] inc))

(reagent/render-component
 [home-page]
 (. js/document (getElementById "app")))

(defonce on-first-load
  (do 
      (on-js-reload)))
