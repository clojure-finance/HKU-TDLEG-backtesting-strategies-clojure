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
            (:use (incanter core stats charts))
            )


(defn parse-double
  "Parse double from string. May return nil."
  [str]
  (try (Double/parseDouble str)
       (catch Exception e nil)))

(defn search-opt
  "Return the line of a given date and option ID."
  [opt-data date optid]
  (loop [remaining opt-data]
    (if (empty? remaining) 
      nil
      (let [first-line (first remaining) next-remaining (rest remaining)] 
        (if (and (= (get first-line :date) date) (= (parse-int (get first-line :optionid)) optid))
          {:optionid (parse-int (get first-line :optionid))
          :volume (parse-int (get first-line :volume))
          :best_offer (parse-double (get first-line :best_offer))
          :best_bid (parse-double (get first-line :best_bid))
          :impl_volatility (parse-double (get first-line :impl_volatility))
          :delta (parse-double (get first-line :delta))
          :gamma (parse-double (get first-line :gamma))
          :vega (parse-double (get first-line :vega))
          :theta (parse-double (get first-line :theta))}
          (recur next-remaining))))))

(defn change-format-opts
  "Change the date format of the option dataset."
  [data]
  (map (fn [line]
    (let [last-date (get line :last_date)
          date (get line :date)
          exdate (get line :exdate)]
        (def last_date_ (str/replace last-date #"/" "-"))
        (def date_ (str/replace date #"/" "-"))
        (def exdate_ (str/replace exdate #"/" "-"))
        (assoc line :last_date last_date_ :date date_ :exdate exdate_)))
    data))


(defn change-format-stock
  "Change the date format of the stock dataset."
  [data]
  (map (fn [line]
    (let [date (get line :date)]
        (def date_ (str/replace date #"/" "-"))
        (assoc line :date date_ )))
    data))

