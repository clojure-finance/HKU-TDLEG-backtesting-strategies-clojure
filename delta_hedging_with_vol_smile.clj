(ns clojure-backtesting.user
  ;;(use '(incanter core stats datasets))
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


(defn check-available-date
  "Check date restriction for selected options."
  [date exdate]
  (let [[year1 month1 day1] (map parse-int (str/split date #"-"))
        [year2 month2 day2] (map parse-int (str/split exdate #"-"))]
    (and (t/before? (t/plus (t/local-date year1 month1 day1) (t/days 6)) (t/local-date year2 month2 day2)) 
          (t/after? (t/plus (t/local-date year1 month1 day1) (t/days 15)) (t/local-date year2 month2 day2)))))

(defn available-opts
  "Filter the options according to trading date."
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
                                                            :theta (parse-double (get first-line :theta))}
                                                            ))
                    (recur next-remaining result-set))))))


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


(defn adj_term
  "Do the regression and return adjustment term dvol/dK."
  [vol_K]
  (let [vol (into [] (map :impl_volatility vol_K))
        K (into [] (map :strike vol_K))]
        (last (:coefs (linear-model vol K)))))

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

(defn option-delta
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
            (recur (inc counter) (+ opt-delta (* (nth weights counter) adj_delta))))
          ;;(recur (inc counter) (+ opt-delta (* (nth weights counter) (+ (nth deltas counter) (* (nth vegas counter) dvol_dK)))))
          ))))

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


(defn -main
  [& args]
;;****************************************************************************************************
  (reset! data-set (add-aprc (change-format-stock (read-csv-row "./resources/stock2020.csv"))))

  (def opts (atom {}))
  (def vol_K (atom {}))
  (def opt-delta (atom 0.0))
  (init-portfolio "2020-01-02" 154000)
  (order "AAPL" 1000)
  (reset! vol_K (vol_strike (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date)))
  (reset! opts (add-weight (available-opts (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date))))
  (reset! opt-delta (option-delta (deref opts) (adj_term (deref vol_K))))

  (doseq [opt (deref opts)]
    (order-opts (get-date) opt (deref opt-delta)))

  (next-date)

  (while (< (compare (get-date) "2020-12-31") 0)
    (reset! vol_K (vol_strike (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date)))
    (reset! opts (add-weight (available-opts (change-format-opts (read-csv-row "./resources/option2020.csv")) (get-date))))
    (reset! opt-delta (option-delta (deref opts) (adj_term (deref vol_K))))
    (println (get-date))
    (loop [remaining (deref opts)]
      (if-not (empty? remaining)
        (let [first-opt (first remaining) rest-opts (rest remaining)]
          (order-opts (get-date) first-opt (deref opt-delta))
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
;;****************************************************************************************************

  ;; (reset! data-set (add-aprc (read-csv-row "./resources/CRSP-extract.csv")))
  ;; ;(reset! data-set (add-aprc (read-csv-row "./resources/data-CRSP-lohi-extract-1000.csv")))
  ;; (init-portfolio "1986-01-09" 2000)
  ;; (let [tot-val (get-in (deref portfolio) [:cash :tot-val])]
  ;;   (println tot-val)
  ;; )

  ;; (order "AAPL" 10)
  ;; ;(order "OMFGA" 10)
  ;; (next-date)

  ;; (update-eval-report (get-date))
  ;; (next-date)
  ;; (update-eval-report (get-date))

  ;; (println (portfolio-daily-ret))
  ;; (next-date)
  ;; (next-date)
  ;; (next-date)
  ;; (next-date)
  ;; (next-date)

  ;; (pprint/print-table (deref order-record))
  ;; (view-portfolio)
  ;; (view-portfolio-record -1)
  ;; (eval-report -1)

  ;; ;; (println (sd-last-n-days "OMFGA" 10))
  ;; ;; (let [prev-close (Double/parseDouble (get (first (get-prev-n-days :PRC 1 "OMFGA")) :PRC))]
  ;; ;;   (println (parabolic-SAR "OMFGA" "non-lazy" 0.2 prev-close))
  ;; ;;   )

  ;; ;; (let [low-price (Double/parseDouble (get-by-key "OMFGA" :BIDLO "non-lazy"))
  ;; ;;       high-price (Double/parseDouble (get-by-key "OMFGA" :ASKHI "non-lazy"))
  ;; ;;       prev-atr (- high-price low-price)]
  ;; ;;   (println (keltner-channel "OMFGA" "non-lazy" 10 prev-atr))
  ;; ;;   )

  ;; ;; (println (force-index "OMFGA" "non-lazy" 20))
;;****************************************************************************************************


  ;; (end-order)

)


