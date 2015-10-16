(ns ehr.core
  (:require
   [ehr.actions :as actions]
   [ehr.util :as util :refer [register app-state reducers dispatch next-tick]]
   [ehr.rxnorm :as rxnorm]
   [ehr.rxpad :as rxpad]
   [clojure.string :as str]
   [reagent.core :as reagent ]))

(enable-console-print!)
(declare on-js-reload)
(str/replace "test\none\ntwo" "\n" "<br/>")

(defn span-formatted [s]
  (-> s
    (str/replace "\n" "<br/>")
    (str/replace " " "&nbsp;")))

(defn clj->editable [a]
  (-> a
      clj->js
      (js/JSON.stringify nil 2)))

(defn editable->clj [e]
  (-> e
      (str/replace #"\s+" " ")
      js/JSON.parse
      js->clj))

(defn normalize-json [e]
  (try
    (-> e editable->clj clj->editable)
    (catch js/Object e nil))) 

(defn save-hook [id value-str]
  (let [hook (editable->clj value-str)]
    (when (not= id (hook "id"))
      (dispatch actions/hook-put id nil))
    (dispatch actions/hook-put (hook "id") hook)))

(defn delete-hook [id]
  (dispatch actions/hook-put id nil))


(defn hook-editor [props hook]
  (let [original (atom (clj->editable hook))
        id (hook "id")
        current (reagent/atom @original)]
    (reagent/create-class
     {
      :component-will-receive-props
      (fn [this [_ props hook]]
        (reset! original (clj->editable hook))
        (reset! current @original))

      :reagent-render
      (fn [props hook]
        [:div {}
         [:button {:onClick #(delete-hook id)} "delete"]
         [:button {:disabled (or (nil? @current) (= @current @original))
                   :onClick #(save-hook id @current)} "save"]
         (when-not @current " invalid")
         [:div {:contentEditable true
                :onInput #(reset! current ( normalize-json (.. % -target -textContent)))
                :dangerouslySetInnerHTML
                {:__html (span-formatted @original)}}]])})))

(defn hook-manager [hook-store]
  [:div
   (for [hook-id (keys (get-in hook-store [:definitions]))]
     [hook-editor {:key hook-id}   (-> hook-store :definitions (get hook-id))])])
   
(defn home-page []
  (let [state @app-state]
    [:div [:h2 "Demo EHR"]
     [rxpad/drug-selector (state :drug-store)]
     [hook-manager (state :hook-store)]
     [:div [:a {:href "#/about"} "go to about page"]]]))


(defn on-js-reload []
    (next-tick util/on-hash-change)
    (swap! app-state update-in [:__figwheel_counter] inc))

(reagent/render-component
 [home-page]
 (. js/document (getElementById "app")))

(defonce on-first-load
  (do (on-js-reload)))

(swap! app-state assoc-in [:hook-store] {:definitions {
                                                        "test-a" {
                                                                 "id" "test-a"
                                                                 "one" "two"
                                                                 }
                                                        "test-b" {"id" "test-b"
                                                                 }
                                                        }})
(register
 actions/hook-put :hooks [:hook-store]
 (fn [hook-store id h]
   (println "Did a hook put on " id h hook-store)
   (if h
     (assoc-in hook-store [:definitions id] h)
     (update-in hook-store [:definitions] dissoc id))))
