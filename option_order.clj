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

(defn update-opts-portfolio
  "Update portfolio according to the option traded."
  [optid quantity bid offer]
  (if-not (contains? (deref portfolio) optid) 
    (let [tot-val (* offer quantity) 
          tot-val-real (* bid quantity)]
      (do 
        (swap! portfolio (fn [curr-port] (conj curr-port [optid {:price bid :aprc offer :quantity quantity :tot-val tot-val-real}])))
        (swap! portfolio assoc :cash {:tot-val (- (get-in (deref portfolio) [:cash :tot-val]) tot-val)})))
    ;; if already has it, just update the quantity
    (if (> (- quantity (get-in (deref portfolio) [optid :quantity])) 0)
      ;;buy more option
      (let [qty (- quantity (get-in (deref portfolio) [optid :quantity]))]
        (do 
          (swap! portfolio assoc optid {:quantity quantity :tot-val (* bid quantity) :price bid :aprc offer})
          (swap! portfolio assoc :cash {:tot-val (- (get-in (deref portfolio) [:cash :tot-val]) (* qty offer))})))
      (let [qty (- (get-in (deref portfolio) [optid :quantity]) quantity)]
        (do 
          (swap! portfolio assoc optid {:quantity quantity :tot-val (* bid quantity) :price bid :aprc offer})
          (swap! portfolio assoc :cash {:tot-val (+ (get-in (deref portfolio) [:cash :tot-val]) (* qty bid))}))))))


(defn order-opts
  "Main functions for ordering options."
  [date opt opt-delta]
  (let [optid (:optionid opt) 
        quantity (Math/round (* (:weight opt) (- 0 (/ 1000 opt-delta)))) 
        bid (* (:best_bid opt) 100) 
        offer (* (:best_offer opt) 100)]
        (update-opts-portfolio optid quantity bid offer)))
