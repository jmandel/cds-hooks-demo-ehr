(ns ehr.actions)

(defmacro defaction
  "Define a new action, whose values is a keyword"
  [action-name]
  `(def ~action-name ~(keyword (name action-name))))
