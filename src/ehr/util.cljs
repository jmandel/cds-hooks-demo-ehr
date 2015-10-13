(ns ehr.util
  (:require
   [ehr.actions :as actions]
   [reagent.core :as reagent :refer [atom]]))

(defn next-tick [f & args]
  (js/window.setTimeout
   (fn [] (apply f args))))

(defonce app-state
  (atom {:drug-store {}}))

(def reducers (atom {}))

(defn register [action key path f]
  (swap! reducers update-in [action]
         (fn [rlist] (->> rlist
                         (filter #(not= key (:key (meta %))))
                         vec)) )
  (let [ new-action (with-meta (vector f path) {:key key})]
    (println "Adding actionw ith eky" (meta new-action))
    (swap! reducers update-in [action]
           (fnil conj [])
           new-action)))

(def ^:dynamic *dispatching* false)

(declare dispatch)

(defn dispatch-simple [tag & args]
  (when *dispatching*
    (throw (js/Error. "Can't perform a nested dispatch!")))
  (binding [*dispatching* true]
    (let [reducer-coll (@reducers tag)
          initial-state @app-state
          next-state
          (reduce 
           (fn [state [next-fn path]]
             (if (empty? path)
               (or (apply next-fn state args) state)
               (update-in state path #(or (apply next-fn % args) %))))
           initial-state
           reducer-coll)]
      (reset! app-state next-state)
      (println "New state" next-state)
      next-state)))

(defn dispatch [tag & args]
  (let [response
        (if (fn? tag)
          (apply tag @app-state args)
          (apply dispatch-simple tag args))]
    (dispatch-simple actions/state-to-url)
    response))

(add-watch
 app-state :populate-url-params
 (fn [key atom old-state new-state]
   (let [old-params (old-state :url-params)
         new-params (new-state :url-params)]
     (when-not (= old-params new-params)
       (let [url-params (->> new-params
                             (filter (fn [[k v]] v))
                             (into {})
                             clj->js
                             js/JSON.stringify)]
         (js/history.replaceState nil nil (str "#" url-params)))))))

(defn on-hash-change [e]
  (let [url-params (-> (js/location.hash.slice 1)
                                     js/JSON.parse
                                     js->clj)]
    (next-tick dispatch actions/url-to-state url-params)))

(js/addEventListener "hashchange" on-hash-change)
