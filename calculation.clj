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

(defn vol_strike
  "Construct strike and volatitity matrix for regression."
  [opt-data date]
  (loop [remaining opt-data
        result-set []]
        (if (empty? remaining)
          result-set
          (let [first-line (first remaining)
                next-remaining (rest remaining)]
                (if (and (= (get first-line :date) date) (not= (parse-double (get first-line :impl_volatility)) nil))
                    (recur next-remaining (conj result-set {:strike (/ (parse-double (get first-line :strike_price)) 1000)
                                                            :impl_volatility (parse-double (get first-line :impl_volatility))}))
                    (recur next-remaining result-set))))))




(defn option-BS-delta
  "Calculate the Black Scholes option delta."
  [opts]
  (let [weights (map :weight opts)
        deltas (map :delta opts)
        vegas (map :vega opts)
        length (count weights)]
        (loop [counter 0 opt-delta 0]
          (if (= counter (- length 1))
          (* opt-delta 100)
          (let [BS_delta (nth deltas counter)]
            (recur (inc counter) (+ opt-delta (* (nth weights counter) BS_delta))))))))


(defn adj_term
  "Do the regression and return adjustment term dvol/dK."
  [vol_K]
  (let [vol (into [] (map :impl_volatility vol_K))
        K (into [] (map :strike vol_K))]
        (last (:coefs (linear-model vol K)))))

(defn option-adj-delta
  "Calculate the adjusted option delta."
  [opts dvol-dK]
  (let [weights (map :weight opts)
        deltas (map :delta opts)
        vegas (map :vega opts)
        length (count weights)]
        (loop [counter 0 opt-delta 0]
          (if (= counter (- length 1))
          (* opt-delta 100)
          (let [adj_delta (+ (nth deltas counter) (* (nth vegas counter) dvol-dK))]
            (recur (inc counter) (+ opt-delta (* (nth weights counter) adj_delta))))))))


(defn add-weight
  "Calculate the weight of each option according to their trading volume."
  [opts]
  (let [tot-volume (float (reduce + (map :volume opts)))]
    (loop [remaining opts
          added-weight-opts []]
          (if (empty? remaining)
          added-weight-opts
          (let [first-line (first remaining)
                next-remaining (rest remaining)]
                (recur next-remaining (conj added-weight-opts (assoc first-line :weight (/ (:volume first-line) tot-volume)))))))))



