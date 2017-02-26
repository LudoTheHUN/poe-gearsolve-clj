(ns poe-gearsolve-clj.core
  (:require [clj-http.client :as client]))

(defn POESESSID-fn [] (read-string (slurp ".POESSID")))

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
    flat-all-tabs-data))

(defn item->kind  [item]
  (let [http-split (clojure.string/split (:icon item) #"/")]
    (cond
      (= (nth http-split 6 nil) "Amulets")  :Amulet
      (= (nth http-split 6 nil) "Belts")    :Belt
      (= (nth http-split 6 nil) "Gems")     :Gem
      (= (nth http-split 6 nil) "Maps")     :Map
      (= (nth http-split 6 nil) "Jewels")   :Jewel
      (= (nth http-split 6 nil) "Currency") :Currency
      (= (nth http-split 6 nil) "ZWFndWVJZCI7TzoxODoi") :Flask
      (= (nth http-split 6 nil) "Divination") :Divination-card
      (= (nth http-split 6 nil) "Rings")      :Ring
      (= (nth http-split 6 nil) "Quivers")    :Quiver

      (= (nth http-split 7 nil) "TwoHandWeapons") :TwoHandedWeapon
      (= (nth http-split 7 nil) "OneHandWeapons") :OneHandWeapon
      (= (nth http-split 7 nil) "Helmets")         :Helmet
      (= (nth http-split 7 nil) "Boots")           :Boot
      (= (nth http-split 7 nil) "Gloves")          :Glove
      (= (nth http-split 7 nil) "BodyArmours")     :BodyArmour
      (= (nth http-split 7 nil) "Shields")         :Shield
      :else :UNKNOWN
      )))

(def armour-set-kinds #{:Amulet :Ring :Helmet :BodyArmour :Belt :Glove :Boot})



(re-find #"([-]?[0-9]*)[\.]?[0-9]" "-234.3 sdsd")
(re-find #"([-]?[0-9]*)[\.]?[0-9]" "25% increased Stun and Block Recovery")
(Float. "34")
(re-matches #"\b([0-9].*)\b" "Adds 4 to 8 Fire Damage to Attacks")


(defn get-numbers [s]
  (let [matcher (re-matcher #"[-]?[0-9]*[\.]?[0-9]" s)]
    [(re-find matcher) (re-find matcher)]))

#_(get-numbers "Adds d-4.8% to 8.1 Fire 56 Damage to Attacks")

(let [a-mod "Adds d-4.8% to 8.1 Fire Damage to Attacks"]
(re-find #"[^0-9]+$" a-mod))

;;Pre word
(let [a-mod "sdfsdf 4.8% to 8.1 Fire Damage to Attacks"]
  (re-find #"^(.*?)[0-9]" a-mod))


(defn filter-items-of-kind [tabs-data kind]
  (filter (fn [i] (= (item->kind i) kind)) tabs-data))

(defn mod-string->mod-kv [a-mod]
  (let [str-get (fn [s] (re-find #"[^0-9]+$" a-mod))
        int-get (fn [s]   (try (Float. (first (get-numbers a-mod)))
                            (catch Exception e :F-ERROR)))
        mod-k (cond
                (= (str-get a-mod) "% increased Stun Duration on Enemies")
                :StunDurationPct

                (= (str-get a-mod) " to Strength")
                :Strength

                (= (str-get a-mod) " to Armour")
                :Armour

                (= (str-get a-mod) " to maximum Energy Shield")
                :EnergyShield

                ;;TODO need to differenciate between the total Energy Shield of item and it's mods

                :else a-mod
                )
        mod-v (if (keyword? mod-k) (int-get a-mod)
                [(str-get a-mod)
                 (int-get int-get)
                 ])
        ]
    {mod-k
     mod-v
     }
    ))

;(mod-string->mod-kv "24% increased Stun Duration on Enemies")
;;  {:StunDurationPct 24}


(defn item->modsmap  [item]
  (let [all-mod-strings (concat
                          (:implicitMods item)
                          (:explicitMods item))
        ]

    ;;TODO need to look at item kinds an either use the mods (for belts rings amulets) or item properties

    (reduce (fn [mod-map a-mod]
             (conj mod-map (mod-string->mod-kv a-mod)) )
            {} all-mod-strings)
    ))

;;Need another function for properties

(comment
  (item->modsmap
    (first (filter-items-of-kind all-tabs-data :Glove)))
  (item->modsmap
    (first (filter-items-of-kind all-tabs-data :Belt)))
  )

(defn find-armout-sets [all-tabs-data armout-set-when-fn]
  (take 100
        (for [Amulet (filter-items-of-kind all-tabs-data :Amulet)
              Ring1 (filter-items-of-kind all-tabs-data :Ring)
              Ring2 (filter-items-of-kind all-tabs-data :Ring)
              Helmet (filter-items-of-kind all-tabs-data :Helmet)
              BodyArmour (filter-items-of-kind all-tabs-data :BodyArmour)
              Belt (filter-items-of-kind all-tabs-data :Belt)
              Glove (filter-items-of-kind all-tabs-data :Glove)
              Boot (filter-items-of-kind all-tabs-data :Boot)
              ]
          {:armout-set
           {:Amulet Amulet
            :Ring1 Ring1 :Ring2 Ring2}}
          )))





(comment
  ;(def tab-data-0 (get-tab-data "LudoTheHUN" "" "Breach" 0))
  ;(def tab-data (get-tab-data "LudoTheHUN" "" "Breach" 9))

  (def all-tabs-data (get-all-tabs
                       "LudoTheHUN"   ;;you account name
                       (POESESSID-fn)   ;;POESESSID, get it from your browser cookies , a 32 char string
                       "Breach"
                       ))


  (filter-items-of-kind all-tabs-data :Belt)
  (filter-items-of-kind all-tabs-data :Glove)
  (last (filter-items-of-kind all-tabs-data :Glove))

  (clojure.pprint/pprint
    (map item->modsmap
         (filter-items-of-kind all-tabs-data :Glove)))


 (count (find-armout-sets all-tabs-data identity))


   (frequencies (filter armour-set-classes (map item->kind all-tabs-data)
                 ))


    (filter (fn [i] (= (item-classifier i) "Divination"))
                   all-tabs-data)


    (map :properties all-tabs-data)


    (clojure.pprint/pprint (take 10  all-tabs-data))




    (count all-tabs-data)

    (keys tab-data)

    (frequencies  (map :properties all-tabs-data
                       ))

    ;; TODO unify item types
    ;; TODO unify item stats
    ;; TODO Encode constraints of a gear-set.
    ;; TODO: constraint optimiser for minimal resistances and shield... points based gear-set search

    )
