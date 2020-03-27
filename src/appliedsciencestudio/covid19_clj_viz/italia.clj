(ns appliedsciencestudio.covid19-clj-viz.italia
  "Following Alan Marazzi's 'The Italian COVID-19 situation' [1] with orthodox Clojure rather than Panthera

  Run it in NextJournal at https://nextjournal.com/alan/getting-started-with-italian-data-on-covid-19

  [1] https://alanmarazzi.gitlab.io/blog/posts/2020-3-19-italy-covid/"
  (:require [clojure.data.csv :as csv]
            [meta-csv.core :as mcsv]
            [clojure.string :as string]))

;; We can get province data out of Italy's CSV data using the orthodox
;; Clojure approach, `clojure.data.csv`:
(def provinces
  (let [hdr [:date :state :region-code :region-name
             :province-code :province-name :province-abbrev
             :lat :lon :cases]
        rows (csv/read-csv (slurp "resources/Italia-COVID-19/dati-province/dpc-covid19-ita-province.csv"))]
    (map zipmap
         (repeat hdr)
         (rest rows))))

;; Or, we could use Nils Grunwald's `meta-csv`, which does the
;; automatic parsing that we often want.

;; First, an Italian-to-English translation mapping for header names.
(def fields-it->en
  {;; For provinces (and some for regions too)
   "data"                    :date
   "stato"                   :state
   "codice_regione"          :region-code
   "denominazione_regione"   :region-name
   "codice_provincia"        :province-code
   "denominazione_provincia" :province-name
   "sigla_provincia"         :province-abbreviation
   "lat"                     :lat
   "long"                    :lon
   "totale_casi"             :cases
   ;; For regions
   "ricoverati_con_sintomi"      :hospitalized
   "terapia_intensiva"           :icu
   "totale_ospedalizzati"        :tot-hospitalized
   "isolamento_domiciliare"      :quarantined
   "totale_attualmente_positivi" :tot-positives
   "nuovi_attualmente_positivi"  :new-positives
   "dimessi_guariti"             :recovered
   "deceduti"                    :dead
   "tamponi"                     :tests})

;; Now we can just read the CSV.
(def provinces2
  (mcsv/read-csv "resources/Italia-COVID-19/dati-province/dpc-covid19-ita-province-latest.csv"
                 {:field-names-fn fields-it->en}))

(comment
  ;;;; Let's examine the data.
  ;; Instead of `pt/names`:
  (keys (first provinces))

  (keys (first provinces2))


  ;;;; I often use an alternative approach when reading CSVs,
  ;;;; transforming rather than replacing the header:
  (let [[hdr & rows] (csv/read-csv (slurp "resources/Italia-COVID-19/dati-province/dpc-covid19-ita-province.csv"))]
    (map zipmap
         (repeat (map (comp keyword #(string/replace % "_" "-")) hdr))
         rows))q


  ;;;; Check the data
  ;; Do we have the right number of provinces?
  (count (distinct (map :province-name provinces))) ;; be sure to evaluate the inner forms as well

  ;; No, there's an extra "In fase di definizione/aggiornamento", or cases not attributed to a province.

  ;; Let's ignore those.
  (remove (comp #{"In fase di definizione/aggiornamento"} :province-name) provinces)

  )


;; Again, we can use the DIY approach with Clojure's standard CSV parser:
(def regions
  (let [hdr [:date :state :region-code :region-name :lat :lon
             :hospitalized :icu :total-hospitalized :quarantined
             :total-positives :new-positives :recovered :dead :tests]
        [_ & rows] (csv/read-csv (slurp "resources/Italia-COVID-19/dati-regioni/dpc-covid19-ita-regioni.csv"))]
    (->> (map zipmap (repeat hdr) rows)
         (map (comp (fn [m] (update m :quarantined #(Integer/parseInt %)))
                    (fn [m] (update m :hospitalized #(Integer/parseInt %)))
                    (fn [m] (update m :icu #(Integer/parseInt %)))
                    (fn [m] (update m :dead #(Integer/parseInt %)))
                    (fn [m] (update m :recovered #(Integer/parseInt %)))
                    (fn [m] (update m :total-hospitalized #(Integer/parseInt %)))
                    (fn [m] (update m :total-positives #(Integer/parseInt %)))
                    (fn [m] (update m :new-positives #(Integer/parseInt %)))
                    (fn [m] (update m :tests #(Integer/parseInt %))))))))

;; Or use `meta-csv` so it comes out as a seq of maps:
(def regions
  (mcsv/read-csv "resources/Italia-COVID-19/dati-regioni/dpc-covid19-ita-regioni.csv"
                 {:field-names-fn fields-it->en}))


;;;; Calculate daily changes
;; Now we want to determine how many new tests were performed each
;; day, and the proportion that were positive.

;; The following is equivalent to `regions-tests`:
(def tests-by-date
  (reduce (fn [acc [date ms]]
            (conj acc {:date date
                       :tests (apply + (map :tests ms))
                       :new-positives (reduce + (map :new-positives ms))}))
          []
          (group-by :date regions)))

(comment
  ;; Take a quick look at the data
  (sort-by :date tests-by-date)

  ;; And calculate our result using Clojure's sequence & collection libraries:
  (reduce (fn [acc [m1 m2]]
            (conj acc (let [daily-tests (- (:tests m2) (:tests m1))]
                        (assoc m2
                               :daily-tests daily-tests
                               :new-by-test (double (/ (:new-positives m2)
                                                       daily-tests))))))
          []
          (partition 2 1 (conj (sort-by :date tests-by-date)
                               {:tests 0})))

  )


(defn conform-to-province-name
  ""
  [provinces]
  (->> (map #(vector (:province-name %) %) provinces)
       (into {})))

(defn add-population-to-province
  ""
  [all-province-data all-province-populations]
  (map #(let [province-to-update (% :province-name)]
          (->> (all-province-populations province-to-update)
               (:population)
               (assoc % :population))) all-province-data))

(defn compute-cases-per-100k [province-data-with-pop]
  (map #(let [cases (% :cases)
              population (% :population)
              calc-cases (fn [x] (double (/ cases x)))
              per-100k (fn [x] (/ x 100000))]
          (->> (if population ((comp calc-cases per-100k) population) nil)
               (assoc % :cases-per-100k))) province-data-with-pop))

(def province-populations
  "From http://www.comuni-italiani.it/province.html. Italy changed how provinces are structured in Sardina in 2016.
   Some are manually updated using the data here: https://en.wikipedia.org/wiki/Provinces_of_Italy"
  (-> (mcsv/read-csv "resources/italy.province-population.csv" {:fields [:province-name :population :abbreviation]})
      conform-to-province-name))

(def province-data (-> (remove (comp #{"In fase di definizione/aggiornamento"} :province-name) provinces2)
                       (add-population-to-province province-populations)
                       (compute-cases-per-100k)
                       (conform-to-province-name)))
