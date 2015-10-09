(ns ehr.rxpad
  (:require [ehr.actions :as actions ]
            [ehr.rxnorm :as rxnorm ]
            [ehr.util :as util :refer [register dispatch]]))

(def steps [:ingredient :components :prescribable :done])
(def next-step (into {} (map vector steps (rest (conj steps (last steps))))))

(defn choices-for [step selection]
  (case step
    :components
    (rxnorm/find-components-by-ingredient selection)
    :prescribable
    (rxnorm/find-prescribable-by-components selection)
    nil))

(defn drug-selected [idx]
  (let [state (dispatch actions/drug-select index)
        step (get-in state [:drug-store :step])
        choices (get-in state [:drug-store :choices])]
    (and (not= :done step)
         (= 1 (count choices))
         (recur 0))))

(let [step-stack (atom nil)]

  (register actions/drug-search :rxpad [:drug-store]
            (fn [drug-store q]
              (reset! step-stack nil)
              (-> (or drug-store {})
                  (assoc-in [:choices :ingredient]
                            (if (< 2 (count q))
                              (rxnorm/find-ingredient-by-name q) []))
                  (assoc :q q)
                  (assoc :step :ingredient)
                  (assoc :selected-row 0))))

  (register actions/drug-step :rxpad [:drug-store]
   (fn [drug-store current selection explicit?]
     (let [drug-store (or drug-store {})
           next (next-step current)
           choices (choices-for next selection)
           response (-> (or  drug-store {})
                        (assoc-in [:decision current] selection)
                        (assoc-in [:choices next] choices)
                        (assoc :selected-row 0)
                        (assoc :step next))
           num-choices (count choices)]
       (and ; and as "when" -- an antipattern?
        (= 1 num-choices)
        (util/next-tick dispatch actions/drug-step next (first choices) false))
       (when explicit? (swap! step-stack conj next))
       (println "so drug step" current "--" selection " -->" next num-choices " So\n" response)
       response)))

  (register actions/drug-select :rxpad [:drug-store]
            (fn ds
              ([drug-store] (ds drug-store (get-in drug-store [:selected-row]) true))
              ([drug-store idx] (ds drug-store idx true))
              ([drug-store idx explicit?]
               (let [drug-store (or drug-store {})
                     current (get drug-store :step :ingredient)]
                 (let [selection (get-in drug-store [:choices current idx])]
                   (util/next-tick dispatch actions/drug-step current selection explicit?)))
               nil)))

  (register actions/drug-previous-step :rxpad [:drug-store]
            (fn [drug-store]
              (let [last-step (first @step-stack)
                    _ (swap! step-stack rest)
                    step (or (first @step-stack) (first steps))]
                (and
                 (= step :ingredient)
                 (empty? (drug-store :q))
                 (util/next-tick dispatch actions/drug-search ""))
                (-> drug-store
                    (assoc :decision {} )
                    (assoc :step step))))))

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
  [:li {:on-click #(dispatch drug-selected}
   (when selected "> ") (:str choice)])

(def key-to-action
  {"Backspace" [actions/drug-previous-step],
   "Enter" [actions/drug-select]
   "ArrowRight" [actions/drug-select]
   "ArrowDown" [actions/drug-cursor-move :down]
   "ArrowUp" [actions/drug-cursor-move :up]})

(defn drug-selector
  [{{:keys [prescribable]} :decision
    :keys [step choices]
    :as drug-store}]
  (println "Drug select" step choices)
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
     (println "So makign choces" choices)
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
     (util/next-tick dispatch actions/drug-step :prescribable
                     {:cui cui :str ((rxnorm/rxnorm "cuiToName") cui)})
     (-> drug-store
         (dissoc :choices)
         (dissoc :q)
         (dissoc :decision)))))
