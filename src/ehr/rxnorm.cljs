(ns ehr.rxnorm
  (:require [clojure.string :as str]))

(defonce rxnorm (js->clj js/db))

(defn with-rxnorm-name [cuis]
  (let [cui-vec (if (vector? cuis) cuis [cuis])]
    {:cui (str/join "," cui-vec)
     :str (str/join " / " (map (rxnorm "cuiToName") cui-vec))}))

(def ingredients
  (let [name-for-cui (rxnorm "cuiToName")]
    (reduce
     (fn [coll k]
       (conj coll (with-rxnorm-name k))) []
       (keys (rxnorm "pillToComponentSets")))))

(defn find-prescribable-by-components [{:keys [cui]}]
  (->> cui
       ((rxnorm "componentSetsToPrescribables"))
       (map with-rxnorm-name)
       (vec)))

(defn compare-arrays [ain bin]
  (let [a (map js/parseFloat ain)
        b (map js/parseFloat bin)
        diff (->> (map vector a b)
                   (filter (partial apply not=))
                   first)]
    (if
      (= a b) 0
      (->> diff (apply >)
          {true 1 false -1}))))

(defn compare-doses [a b]
  (let [nums #(re-seq #"[\d\.]+" (:str %))]
    (compare-arrays (nums a) (nums b))))

(defn find-components-by-ingredient [{:keys [cui]}]
  (->> cui
       ((rxnorm "pillToComponentSets"))
       (map with-rxnorm-name)
       (sort compare-doses)
       (vec)))

(defn find-ingredient-by-name
  ( [q] (find-ingredient-by-name q 30))
  ( [q limit]
    (let [search-words (-> q (str/replace #"\s+" ".*\\s"))
          search-re (re-pattern (str "(?i).*(^|\\s)" search-words ".*"))]
      (->> ingredients
           (filter #(re-matches search-re (:str %)))
           (sort-by #(count (:str %)))
           (take limit)
           vec))))
