(ns ehr.actions
  (:require-macros [ehr.actions :refer [defaction]])
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)
(defaction drug-search)
(defaction drug-step)
(defaction drug-select)
(defaction drug-previous-step)
(defaction drug-pick)
(defaction drug-cursor-move)
(defaction state-to-url)
(defaction url-to-state)
