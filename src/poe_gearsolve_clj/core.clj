(ns poe-gearsolve-clj.core
  (:require [clj-http.client :as client]))

(defn POESESSID-fn [] (read-string (slurp ".POESSID")))

(defn get-tab-data [accountName POESESSID league tabIndex]
  (Thread/sleep 500) ;;Prevent API spamming
  (:body (client/get
           (str "https://www.pathofexile.com/character-window/get-stash-items?accountName=" accountName "&tabIndex=" tabIndex "&league=" league "&tabs=0")
           {:cookies {"POESESSID" {:value POESESSID}}
            :accept  :json
            :as      :json
            :socket-timeout 10000
            :conn-timeout 10000
            }
           )))


(defn get-characters [accountName POESESSID league]
  (filter (fn [c] (= (:league c) league))
          (:body (client/post "https://www.pathofexile.com/character-window/get-characters"
                              {:cookies {"POESESSID" {:value POESESSID}}
                               :form-params {:accountName accountName}
                               :content-type :json
                               :accept  :json
                               :as      :json
                               :socket-timeout 10000
                               :conn-timeout 10000}
                              ))))

;(get-characters "LudoTheHUN" (POESESSID-fn) "Standard")

(defn get-character-items [accountName POESESSID league character]
  (Thread/sleep 500) ;;Prevent API spamming
  (:body (client/post "https://www.pathofexile.com/character-window/get-items"
                      {:cookies {"POESESSID" {:value POESESSID}}
                       :form-params {:accountName accountName :character character}
                       :accept  :json
                       :as      :json
                       :socket-timeout 10000
                       :conn-timeout 10000
                       }
                      )))

;(get-character-items "LudoTheHUN" (POESESSID-fn) "Standard" "LudoTheBreach")


(defn get-all-tabs-items [accountName POESESSID league]
  (let [number-of-tabs (:numTabs (get-tab-data accountName POESESSID league 0))
        all-tabs-data (map (fn [tab-number]
                             (conj {:tab-number tab-number}
                                   (get-tab-data accountName POESESSID league tab-number))) (range 1 number-of-tabs)
                           )
        flat-all-tabs-data (reduce (fn [xs x]
                                     (concat xs (map #(conj % {:tab-number (:tab-number x)
                                                               :location (:tab-number x)
                                                               :at :tab-number}) (:items x)))
                                     ) [] all-tabs-data)]
    flat-all-tabs-data))


(defn get-all-characters-items [accountName POESESSID league]
 ;TODO test!
  (let [characters (map :name (get-characters accountName POESESSID league))
        all-character-items (map (fn [character] (get-character-items accountName POESESSID league character))
                                 characters)

        flat-all-character-data (reduce
                                  (fn [xs x]
                                    (concat xs (map #(conj % {:characterName (:name (:character x))
                                                              :location (:name (:character x))
                                                              :at :characterName}) (:items x))))
                                  [] all-character-items)]
    flat-all-character-data))

;(def all-char-items (get-all-characters-items "LudoTheHUN" (POESESSID-fn) "Standard"))


(defn get-all-items-data
  "scrapes all of items!"
  [accountName POESESSID league]
  (concat
    (get-all-tabs-items accountName POESESSID league)
    (get-all-characters-items accountName POESESSID league)))


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
  (let [matcher (re-matcher #"[-]?[0-9]*[\.]?[0-9]?[0-9]" s)
        to-float (fn [s-num] (try (Float. s-num) (catch Exception e nil)))
        ]
    [(to-float (re-find matcher))
     (to-float (re-find matcher))
     ]))

(comment
  (get-numbers "Adds d-4.% to -8.1 Fire Damage to Attacks")
  (get-numbers "Adds d-4% to -8.1 Fire Damage to Attacks")
  (get-numbers "Adds d-4.8% to -8.1 Fire Damage to Attacks")
  (get-numbers "Adds Fire Damage to Attacks")
  (get-numbers "Adds d-4.81% to -8.10 Fire Damage to Attacks")
  (get-numbers "Adds d-4.81% to -8.15 Fire Damage to Attacks")
  )

(defn get-post-num-s [s]
  (re-find #"[^0-9]+$" s))

#_(get-post-num-s "Adds d-4.8% to 8.1 Fire Damage to Attacks")
#_(get-post-num-s "Adds d- Fire Damage to Attacks")
#_(get-post-num-s "Adds d-4.8% to 8.10 Fire Damage to Attacks")


(defn get-pre-num-s [s]
  (let [match (re-find #"^(.*?)[0-9]" s)]
    (if match (second match) match)))

#_(get-pre-num-s "sdfsdf 4.8% to 8.1 Fire Damage to Attacks")
#_(get-pre-num-s "sdfsdf Fire Damage to Attacks")


(defn filter-items-of-kind [tabs-data kind]
  (filter (fn [i] (= (item->kind i) kind)) tabs-data))

(defn mod-string->mod-kv [a-mod]
  (let [mod-k (cond

                (= (get-post-num-s a-mod) " to Strength")
                :Strength
                (= (get-post-num-s a-mod) " to Intelligence")
                :Intelligence
                (= (get-post-num-s a-mod) " to Dexterity")
                :Dexterity

                (= (get-post-num-s a-mod) " to Armour")
                :Armour-mod-add
                (= (get-post-num-s a-mod) " to maximum Energy Shield")
                :EnergyShield-mod-add
                (= (get-post-num-s a-mod) " to Evasion Rating")
                :EvasionRating-mod-add

                (= (get-post-num-s a-mod) "% increased Armour")
                :Armour-mod-pct-increase
                (= (get-post-num-s a-mod) "% increased Energy Shield")
                :EnergyShield-mod-pct-increase
                (= (get-post-num-s a-mod) "% increased Evasion Rating")
                :EvasionRating-mod-pct-increase

                (= (get-post-num-s a-mod) "% to Cold Resistance")
                :ColdResist
                (= (get-post-num-s a-mod) "% to Fire Resistance")
                :FireResist
                (= (get-post-num-s a-mod) "% to Lightning Resistance")
                :LightningResist

                (= (get-post-num-s a-mod) "% to Chaos Resistance")
                :ChaosResist

                ;;TODO need to differenciate between the total Energy Shield of item and it's mods

                :else (get-post-num-s a-mod)
                )
        mod-v ;(if (keyword? mod-k) [(first (get-numbers a-mod)) (second (get-numbers a-mod))]
        [(first (get-numbers a-mod))
         (second (get-numbers a-mod))
         #{(get-pre-num-s a-mod)}
         #{(get-post-num-s a-mod)}
         ]
        ;)
        ]
    ;;TODO
    ;" to all Attributes"
    ;" to Strength and Dexterity"
    ;" to Dexterity and Intelligence"
    ;" to all Attributes"
    ;"% to Fire and Lightning Resistances"
    ;and all those things....
    (cond (= mod-k " to all Attributes")
          {:Strength mod-v :Intelligence mod-v :Dexterity mod-v}
          (= mod-k " to Strength and Dexterity")
          {:Strength mod-v :Dexterity mod-v}
          (= mod-k " to Dexterity and Intelligence")
          {:Dexterity mod-v :Intelligence mod-v}
          (= mod-k " to Strength and Intelligence")
          {:Strength mod-v :Intelligence mod-v}

          (= mod-k "% to all Elemental Resistances")
          {:ColdResist mod-v :FireResist mod-v :LightningResist mod-v}
          (= mod-k "% to Cold and Lightning Resistances")
          {:ColdResist mod-v :LightningResist mod-v}
          (= mod-k "% to Fire and Cold Resistances")
          {:ColdResist mod-v :FireResist mod-v}
          (= mod-k "% to Fire and Lightning Resistances")
          {:FireResist mod-v :LightningResist mod-v}

          (= mod-k "% increased Armour and Evasion")
          {:Armour-mod-pct-increase mod-v :EvasionRating-mod-pct-increase mod-v}
          (= mod-k "% increased Evasion and Energy Shield")
          {:EvasionRating-mod-pct-increase mod-v :EnergyShield-mod-pct-increase mod-v}
          (= mod-k "% increased Armour and Energy Shield")
          {:Armour-mod-pct-increase mod-v :EnergyShield-mod-pct-increase mod-v}



          :else
          {mod-k
           mod-v
           }
          )))

(defn item->modsmap  [item]
  (let [all-mod-strings (concat
                          (:implicitMods item)
                          (:explicitMods item))]
    ;;TODO need to look at item kinds an either use the mods (for belts rings amulets) or item properties... NO
    ;; Actually, jewlery simply don't have properties (model them as zero value)... We use the qulity  formulay to compute out the result
    (reduce (fn [mod-map a-mod]
              ;(conj mod-map (mod-string->mod-kv a-mod)
              (println mod-map a-mod)

              (merge-with (fn [mod-v1 mod-v2]
                            (let [[mod-v1-n1 mod-v1-n2 mod-v1-s1 mod-v1-s2] mod-v1
                                  [mod-v2-n1 mod-v2-n2 mod-v2-s1 mod-v2-s2] mod-v2]
                              [(if (and mod-v1-n1 mod-v2-n1)
                                 (+ mod-v1-n1 mod-v2-n1))
                               (if (and mod-v1-n2 mod-v2-n2)
                                 (+ mod-v1-n1 mod-v2-n1))
                               (clojure.set/intersection mod-v1-s1 mod-v2-s1)
                               (clojure.set/intersection mod-v1-s2 mod-v2-s2)]))
                          mod-map (mod-string->mod-kv a-mod))

              ;;ERROR HERE
              ;;TODO WIP HERE!
              ;;TODO need to sum values as we merge mod-maps
              )
            {} all-mod-strings)
    ))

(comment
  (item->modsmap (nth (filter-items-of-kind all-tabs-data :Glove) 2))
  (item->modsmap (nth (filter-items-of-kind all-tabs-data :Glove) 0))
  (item->modsmap (nth (filter-items-of-kind all-tabs-data :BodyArmour) 0))
  )

(defn property->prop-kv [property]
  (let [property-key (cond (= "Evasion Rating" (:name property))
                           :EvasionRating-prop
                           (= "Armour" (:name property))
                           :Armour-prop
                           (= "Energy Shield" (:name property))
                           :EnergyShield-prop
                           (= "Quality" (:name property))
                           :Quality
                           :else
                           (:name property)
                           )
        prop-value (first (first (:values property)))
        property-val (if prop-value
                       (vec (concat
                              (get-numbers prop-value)
                              [#{""} #{(:name property)}]))
                       [nil nil #{""} #{(:name property)}])]

    {property-key property-val}))


#_(sort-by (fn [z] ;(first (second z))
             (first (second (first (vec z)))))
           (flatten (map
                      (fn [i] (map property->prop-kv (:properties i)))
                      (filter-items-of-kind all-tabs-data :Helmet))))

(defn item->propmap [item]
  (reduce conj {}
          (map property->prop-kv (:properties item)))

  )



(comment
  (item->modsmap (first (filter-items-of-kind all-tabs-data :Boot)))
  (item->propmap (first (filter-items-of-kind all-tabs-data :Boot)))
  (item->propmap (first (filter-items-of-kind all-tabs-data :TwoHandedWeapon)))
  )

;; http://pathofexile.gamepedia.com/Quality
;(100 base armour + 50 armour) * (1 + 1/100*(100% increased armour + 20% quality)) = 150 * 2.2 = 330 armour
(/
  (* (+ 100.0 50) (+ 1 (* 1/100 (+ 100 20))))
  (* (+ 100.0 50) (+ 1 (* 1/100 (+ 100 0))))
  )

(/
  (* (+ 100.0 50) (+ 1 (* 1/100 (+ 0 20))))
  (* (+ 100.0 50) (+ 1 (* 1/100 (+ 0 0))))
  )

(- (/ 330.0 (+ 1 (* 1/100 (+ 100 20)))) 50)
(- (/ 300.0 (+ 1 (* 1/100 (+ 100 0)))) 50)
;;;TODO reverse compute base property, then compute forward the maximum property under maximum (20) quality .
  ;... will need to bring properties and mod together, could be nice way to capture belts not having properties (zero base value)


;;Need another function for properties

(comment
  (item->modsmap
    (first (filter-items-of-kind all-tabs-data :Glove)))
  (item->modsmap
    (first (filter-items-of-kind all-tabs-data :Belt)))
  )

(defn find-armout-sets [all-tabs-data armout-set-when-fn]
  (take 100
        (for [Amulet     (filter-items-of-kind all-tabs-data :Amulet)
              Ring1      (filter-items-of-kind all-tabs-data :Ring)
              Ring2      (filter-items-of-kind all-tabs-data :Ring)
              Helmet     (filter-items-of-kind all-tabs-data :Helmet)
              BodyArmour (filter-items-of-kind all-tabs-data :BodyArmour)
              Belt       (filter-items-of-kind all-tabs-data :Belt)
              Glove      (filter-items-of-kind all-tabs-data :Glove)
              Boot       (filter-items-of-kind all-tabs-data :Boot)
              ]
          ;;TODO aggregate function that gives total values for all items
          {:armout-set
           {:Amulet Amulet
            :Ring1 Ring1
            :Ring2 Ring2
            ;;TODO !!!
            }}
          )))





(comment
  ;(def tab-data-0 (get-tab-data "LudoTheHUN" "" "Breach" 0))
  ;(def tab-data (get-tab-data "LudoTheHUN" "" "Breach" 9))

  (def all-tabs-data (get-all-items-data
                       "LudoTheHUN"   ;;you account name
                       (POESESSID-fn)   ;;POESESSID, get it from your browser cookies , a 32 char string
                       "Standard"
                       ))

  (count all-tabs-data)

  (frequencies (map item->kind all-tabs-data))

  (frequencies (map :location (filter-items-of-kind all-tabs-data :Gem)))

  (filter-items-of-kind all-tabs-data :Glove)
  (last (filter-items-of-kind all-tabs-data :Glove))

  (clojure.pprint/pprint
    (take 500
          (map item->modsmap
               (filter-items-of-kind all-tabs-data :Ring))))

  ;;All mods
  (clojure.pprint/pprint
    (sort-by str
             (map identity
                  (frequencies
                    (flatten
                      (map keys
                           (map item->modsmap
                                ;(filter-items-of-kind all-tabs-data :Ring))))))))
                                all-tabs-data)))))))

  ;;Find item by mod-k
  ;;Find items with a mod
  (clojure.pprint/pprint
    (filter (fn [mod-map-with-item] (contains? (set (keys mod-map-with-item)) "% more Armour"))
            (flatten
              (map (fn [item] (conj (item->modsmap item) {:item item}))
                   ;(filter-items-of-kind all-tabs-data :Ring))))))))
                   all-tabs-data))))

  ;;Mods with something in their nama
  (clojure.pprint/pprint
    (map first
         (filter (fn [[k v]] (re-find #"Attack " (str k)))
                 (reduce conj {}
                         (map item->modsmap
                              ;(filter-items-of-kind all-tabs-data :Ring))))))))
                              all-tabs-data)))))


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
