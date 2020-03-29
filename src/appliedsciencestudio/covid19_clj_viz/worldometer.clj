(ns appliedsciencestudio.covid19-clj-viz.worldometer
  (:require [hickory.core :as hick]
            [hickory.select :as s]
            [clj-http.client :as client]
            [jsonista.core :as json]
            [clojure.set :refer [rename-keys]]
            [oz.core :as oz]))

;;;; Scraping data
(def worldometers-page
  "We want this data, but it's only published as HTML."
  (-> (client/get "http://www.worldometers.info/coronavirus/")
      :body hick/parse
      hick/as-hickory))

(defn deepest-content
  "Drill down to the deepest content node."
  [node]
  (if-let [content (or (:content node) (:content (first node)))]
    (deepest-content content)
    (cond (vector? node) (apply str (filter string? node))
          (map? node) nil
          :else node)))

(def headers
  (->> (s/select (s/tag :thead) worldometers-page)
       first
       (s/select (s/tag :tr))
       first
       (s/select (s/tag :th))
       (map deepest-content)))

(def dataset
  (->> (s/select (s/tag :tbody) worldometers-page)
       first
       (s/select (s/tag :tr))
       (map (fn [row]
              (zipmap headers (map deepest-content (s/select (s/tag :td) row)))))))

(def world-cases
  "Total cases of COVID19 by country"
  (->> dataset
       (map #(select-keys % ["Country,Other" "TotalCases"]))
       (map #(rename-keys % {"Country,Other" :country, "TotalCases" :total-cases}))
       (map #(update % :country (fn [country]
                                  (case country
                                    "Bahamas" "The Bahamas"
                                    "USA" "United States of America"
                                    "S. Korea" "South Korea"
                                    "Timor-Leste" "East Timor"
                                    "Congo" "Democratic Republic of the Congo"
                                    "Guinea-Bissau" "Guinea Bissau"
                                    "Czechia" "Czech Republic"
                                    "UK" "United Kingdom"
                                    "North Macedonia" "Macedonia"
                                    "Serbia" "Republic of Serbia"
                                    country))))
       (map #(update % :total-cases (fn [total-cases]
                                      (apply str (filter (fn [x] (Character/isDigit x)) total-cases)))
                     ))))

(def world-geojson-with-data
  (update (json/read-value (java.io.File. "resources/public/public/data/world.geo.json")
                           (json/object-mapper {:decode-key-fn true}))
          :features
          (fn [features]
            (mapv (fn [feature]
                    (let [cases (:total-cases (first (filter (comp #{(:geounit (:properties feature))} :country) world-cases)))]
                      (assoc feature
                             :Country (:geounit (:properties feature))
                             :Cases cases)))
                  features))))

(oz/start-server! 8082)

(def oz-config
  "Default settings for Oz visualizations"
  (let [font "IBM Plex Mono"]
    {:config {:style {:cell {:stroke "transparent"}}
              :legend {:labelFont font
                       :labelFontSize 12
                       :titleFont "IBM Plex Mono"
                       :gradientThickness 40}
              :axis {:labelFont font
                     :titleFont font
                     :titleFontSize 20}}
     :title {:font "IBM Plex Sans"
             :fontSize 16
             :anchor "middle"}}))

(def map-dimensions
  {:width 550 :height 700})

(oz/view!
 (merge-with merge oz-config map-dimensions
             {:title {:text "Total cases of COVID-19 by country"}
              :data {:name "south-america"
                     :values world-geojson-with-data
                     :format {:property "features"}}
              :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
              :encoding {:color {:field "Cases"
                                 :type "quantitative"
                                 :scale {:range ["#fde5d9" "#a41e23"]}}
                         :tooltip [{:field "Country" :type "nominal"}
                                   {:field "Cases" :type "quantitative"}]}
              :selection {:highlight {:on "mouseover" :type "single"}}}))