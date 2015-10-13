(ns ehr.rxpad
  (:require [ehr.actions :as actions ]
            [ehr.rxnorm :as rxnorm ]
            [ehr.util :as util :refer [register dispatch app-state]]))

(def steps [:ingredient :components :prescribable :done])
(def next-step (into {} (map vector steps (rest (conj steps (last steps))))))

(defn choices-for [step selection]
  (case step
    :components
    (rxnorm/find-components-by-ingredient selection)
    :prescribable
    (rxnorm/find-prescribable-by-components selection)
    nil))

(register actions/drug-search :rxpad [:drug-store]
          (fn [drug-store q]
            (-> (or drug-store {})
                (assoc :step-stack nil)
                (assoc-in [:choices :ingredient]
                          (if (>= (count q) 1)
                            (rxnorm/find-ingredient-by-name q) []))
                (assoc :q q)
                (assoc :step :ingredient)
                (assoc :selected-row 0))))

(register actions/drug-step :rxpad [:drug-store]
          (fn ds
            ( [drug-store selection explicit?]
              (ds drug-store (get-in drug-store [:step]) selection explicit?))
            ( [drug-store current selection explicit?]
              (println "drug-step " drug-store current selection explicit?)
              (let [drug-store (or drug-store {})
                    selection (if (= :by-keyboard selection)
                                (-> drug-store
                                    :selected-row
                                    ((get-in drug-store [:choices current])))
                                selection)
                    next (next-step current)
                    choices (choices-for next selection)
                    stack (get-in drug-store [:step-stack])
                    stack (if explicit? (conj stack next) stack)
                    response (-> drug-store
                                 (assoc-in [:decision current] selection)
                                 (assoc-in [:choices next] choices)
                                 (assoc :selected-row 0)
                                 (assoc-in [:step-stack] stack)
                                 (assoc :step next))
                    num-choices (count choices)]
                (if (= 1 num-choices)
                  (recur response next (first choices) false)
                  response)))))

(register actions/drug-previous-step :rxpad [:drug-store]
          (fn [drug-store]
            (let [
                  step-stack (get-in drug-store [:step-stack])
                  last-step (first step-stack)
                  step-stack (rest step-stack)
                  step (or (first step-stack) (first steps))]
              (-> drug-store
                  (assoc :step-stack step-stack)
                  (assoc :decision {} )
                  (assoc :step step)))))

(defn guard [lower upper]
  (fn [num]
    (cond
      (<= lower num (dec upper)) num
      (< num lower) lower 
      :else (dec upper))))

(register actions/drug-cursor-move :rxpad [:drug-store]
          (fn [drug-store dir]
            (let [step (get-in drug-store [:step])
                  choices (get-in drug-store [:choices step])
                  bounded (guard 0 (count choices))
                  directions {:up (comp bounded dec)
                              :down (comp bounded inc)}]
              (update-in drug-store
                         [:selected-row]
                         (directions dir)))))

(defn drug-row [{:keys [key index choice selected]}]
  [:li {:on-click #(dispatch actions/drug-step choice true)}
   (when selected "> ") (:str choice)])

(def key-to-action
  {"Backspace" [actions/drug-previous-step],
   "Enter" [actions/drug-step :by-keyboard true]
   "ArrowRight" [actions/drug-select]
   "ArrowDown" [actions/drug-cursor-move :down]
   "ArrowUp" [actions/drug-cursor-move :up]})

(defn drug-selector
  [{{:keys [prescribable]} :decision
    :keys [step choices]
    :as drug-store}]
  [:div
   [:input {:type "text"
            :id "drug-name"
            :value (drug-store :q)
            :onKeyDown
            (fn [e]
              (when (and
                     (= e.key "Backspace")
                     (not= :ingredient (drug-store :step)))
                (.preventDefault e))
              (when-some [a (key-to-action e.key)]
                (apply dispatch a)))
            :on-change #(dispatch actions/drug-search  (.. % -target -value))}]
   (let [choices (get-in drug-store [:choices step])]
     [:span
      [:ul
       (for [[i {:keys [cui] :as choice}] (map-indexed vector choices)]
         [drug-row {
                    :key cui
                    :index i
                    :choice choice
                    :selected (= i (drug-store :selected-row))
                    }])]
      (when prescribable
        [:em (:str prescribable)])])])

(register
 actions/state-to-url :rxpad []
 (fn [state q]
   (let [current-cui (get-in state [:drug-store :decision :prescribable :cui])]
     (-> state
         (assoc-in [:url-params :drug] current-cui)))))

(register
   actions/url-to-state :rxpad [:drug-store]
   (fn [drug-store url-params]
     (when-let [cui (url-params "drug")]
       (let [decision {:cui cui :str ((rxnorm/rxnorm "cuiToName") cui)}]
         (-> drug-store
             (dissoc :choices)
             (dissoc :q)
             (dissoc :decision)
             (assoc-in [:decision :prescribable] decision))))))
