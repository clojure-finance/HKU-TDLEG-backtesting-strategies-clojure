(ns clojure-backtesting.user
  (:require ;[clojure.test :refer :all]
            [clojure-backtesting.data :refer :all]
            [clojure-backtesting.data-management :refer :all]
            [clojure-backtesting.portfolio :refer :all]
            [clojure-backtesting.order :refer :all]
            [clojure-backtesting.evaluate :refer :all]
            [clojure-backtesting.plot :refer :all]
            [clojure-backtesting.counter :refer :all]
            [clojure-backtesting.large-data :refer :all]
            [clojure-backtesting.parameters :refer :all]
            [clojure-backtesting.indicators :refer :all]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clj-time.core :as clj-t]
            [clojure.edn :as edn]
            [java-time :as t]
            [clojupyter.kernel.version :as ver]
            )(:gen-class)
            (:use (incanter core stats charts)))

(defn -main
  [& args]
  (reset! data-set (add-aprc (change-format-stock (read-csv-row "./resources/stock2020.csv"))))

  (def opts (atom {}))
  (def opt-BS-delta (atom 0.0))
  (init-portfolio "2020-01-02" 154000)
  (order "AAPL" 1000)
  (reset! vol_K (vol_strike (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date)))
  (reset! opts (add-weight (available-opts (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date))))
  (reset! opt-BS-delta (option-BS-delta (deref opts)))

  (doseq [opt (deref opts)]
    (order-opts (get-date) opt (deref opt-delta)))

  (next-date)

  (while (< (compare (get-date) "2020-12-31") 0)
    (reset! vol_K (vol_strike (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date)))
    (reset! opts (add-weight (available-opts (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date))))
    (reset! opt-BS-delta (option-BS-delta (deref opts)))
    (println (get-date))
    (loop [remaining (deref opts)]
      (if-not (empty? remaining)
        (let [first-opt (first remaining) rest-opts (rest remaining)]
          (order-opts (get-date) first-opt (deref opt-BS-delta))
          (recur rest-opts))))
    (loop [remaining (keys (rest (deref portfolio)))]
      (if-not (empty? remaining)
      (let [first-tic (first remaining) rest-tic (rest remaining)]
        (if-not (or (= first-tic "AAPL") (= first-tic :cash))
          (if-not (contains? (set (map :optionid (deref opts))) first-tic)
           (do 
            (swap! portfolio assoc :cash {:tot-val (+ (get-in (deref portfolio) [:cash :tot-val]) (get-in (deref portfolio) [first-tic :tot-val]))})
            (swap! portfolio dissoc first-tic))))
        (recur rest-tic))))
    (view-portfolio)
    (update-eval-report (get-date))
    (next-date))
    (eval-report -1)
  (end-order))
