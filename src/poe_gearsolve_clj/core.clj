(ns poe-gearsolve-clj.core
  (:require [clj-http.client :as client]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn get-tab-data [accountName POESESSID league tabIndex]
  (:body (client/get
           (str "https://www.pathofexile.com/character-window/get-stash-items?accountName=" accountName "&tabIndex=" tabIndex "&league=Breach&tabs=0")
           {:cookies {"POESESSID" {:value POESESSID}}
            :accept  :json
            :as      :json}
           )))

(defn get-all-tabs [accountName POESESSID league]
  (let [number-of-tabs (:numTabs (get-tab-data accountName POESESSID league 0))
        all-tabs-data (map (fn [tab-number]
                             (conj {:tab-number tab-number}
                                   (get-tab-data accountName POESESSID league tab-number))) (range 1 number-of-tabs)
                           )
        flat-all-tabs-data (reduce (fn [xs x]
                                     (concat xs (map #(conj % {:tab-number (:tab-number x)}) (:items x)))
                                     ) [] all-tabs-data)]
    flat-all-tabs-data
    ))

(comment
  ;(def tab-data-0 (get-tab-data "LudoTheHUN" "" "Breach" 0))
  ;(def tab-data (get-tab-data "LudoTheHUN" "" "Breach" 9))

  (def all-tabs-data (get-all-tabs
                       "LudoTheHUN"   ;;you account name
                       ""   ;;POESESSID, get it from your browser cookies , a 32 char string
                       "Breach"
                       ))


  (count all-tabs-data)

  (keys tab-data)

  (frequencies  (map :properties all-tabs-data
       ))

  ;; TODO unify item types
  ;; TODO unify item stats
  ;; TODO Encode constraints of a gear-set.
  ;; TODO: constraint optimiser for minimal resistances and shield... points based gear-set search

  )