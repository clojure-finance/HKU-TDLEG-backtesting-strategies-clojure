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
            )(:gen-class))


(defn parse-double
  "Parse double from string. May return nil."
  [str]
  (try (Double/parseDouble str)
       (catch Exception e nil)))

(defn search-opt
  "search a specific option in the option dataset"
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
  "change the date format of the option dataset"
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
  "change the date format of the stock dataset"
  [data]
  (map (fn [line]
    (let [date (get line :date)]
        (def date_ (str/replace date #"/" "-"))
        (assoc line :date date_ )))
    data))

(defn check-available-date
  "check if the time to expiration satisfies the filtering rule"
  [date exdate]
  (let [[year1 month1 day1] (map parse-int (str/split date #"-"))
        [year2 month2 day2] (map parse-int (str/split exdate #"-"))]
    (and (t/before? (t/plus (t/local-date year1 month1 day1) (t/days 6)) (t/local-date year2 month2 day2)) 
          (t/after? (t/plus (t/local-date year1 month1 day1) (t/days 15)) (t/local-date year2 month2 day2)))))

(defn available-opts
  "return filtered options"
  [opt-data date]
  (loop [remaining opt-data
        result-set []]
        (if (empty? remaining)
          (take 5 (sort-by :volume >  result-set))
          (let [first-line (first remaining)
                next-remaining (rest remaining)]
                (if (and (= (get first-line :date) date) 
                          (check-available-date (get first-line :date) (get first-line :exdate)))
                    (recur next-remaining (conj result-set {:optionid (parse-int (get first-line :optionid))
                                                            :volume (parse-int (get first-line :volume))
                                                            :best_offer (parse-double (get first-line :best_offer))
                                                            :best_bid (parse-double (get first-line :best_bid))
                                                            :impl_volatility (parse-double (get first-line :impl_volatility))
                                                            :delta (parse-double (get first-line :delta))
                                                            :gamma (parse-double (get first-line :gamma))
                                                            :vega (parse-double (get first-line :vega))
                                                            :theta (parse-double (get first-line :theta))}))
                    (recur next-remaining result-set))))))

(defn add-weight
  "calculate trading weight according to daily trading volume"
  [opts]
  (let [tot-volume (float (reduce + (map :volume opts)))]
    (loop [remaining opts
          added-weight-opts []]
          (if (empty? remaining)
          added-weight-opts
          (let [first-line (first remaining)
                next-remaining (rest remaining)]
                (recur next-remaining (conj added-weight-opts (assoc first-line :weight (/ (:volume first-line) tot-volume)))))))))

(defn option-delta
  "return composite delta for options"
  [opts]
  (let [weights (map :weight opts)
        deltas (map :delta opts)
        length (count weights)]
        (loop [counter 0 opt-delta 0]
          (if (= counter (- length 1))
          opt-delta
          (recur (inc counter) (+ opt-delta (* (nth weights counter) (nth deltas counter))))))))

(defn update-opts-portfolio
  [optid quantity bid offer]
  (if-not (contains? (deref portfolio) optid) 
    (let [tot-val (* offer quantity) 
          tot-val-real (* bid quantity)]
      (do 
        (swap! portfolio (fn [curr-port] (conj curr-port [optid {:price bid :aprc offer :quantity quantity :tot-val tot-val-real}])))
        (swap! portfolio assoc :cash {:tot-val (- (get-in (deref portfolio) [:cash :tot-val]) tot-val)})))
    ;; if already has it, just update the quantity
    (if (> (- quantity (get-in (deref portfolio) [optid :quantity])) 0)
      ;; buy more option
      (let [qty (- quantity (get-in (deref portfolio) [optid :quantity]))]
        (do 
          (swap! portfolio assoc optid {:quantity quantity :tot-val (* bid quantity) :price bid :aprc offer})
          (swap! portfolio assoc :cash {:tot-val (- (get-in (deref portfolio) [:cash :tot-val]) (* qty offer))})))
      (let [qty (- (get-in (deref portfolio) [optid :quantity]) quantity)]
        (do 
          (swap! portfolio assoc optid {:quantity quantity :tot-val (* bid quantity) :price bid :aprc offer})
          (swap! portfolio assoc :cash {:tot-val (+ (get-in (deref portfolio) [:cash :tot-val]) (* qty bid))}))))))

(defn order-opts
  [date opt]
  (let [optid (:optionid opt) 
        quantity (Math/round (* (:weight opt) 10)) 
        bid (* (:best_bid opt) 100) 
        offer (* (:best_offer opt) 100)]
        (update-opts-portfolio optid quantity bid offer)))



(defn -main
  [& args]
  (reset! data-set (add-aprc (change-format-stock (read-csv-row "./resources/stock2020.csv"))))
  (def opts (atom {}))
  (init-portfolio "2020-01-02" 154000)
  (order "AAPL" 1000)
  (reset! opts (add-weight (available-opts (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date))))
  (doseq [opt (deref opts)]
    (order-opts (get-date) opt))
  (next-date)
  (while (< (compare (get-date) "2020-12-31") 0)
    (reset! opts (add-weight (available-opts (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date))))
    (println (get-date))
    (loop [remaining (deref opts)]
      (if-not (empty? remaining)
        (let [first-opt (first remaining) rest-opts (rest remaining)]
          (order-opts (get-date) first-opt)
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



