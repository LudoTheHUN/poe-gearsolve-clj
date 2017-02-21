# poe-gearsolve-clj

A Clojure library designed to pick the best POE item set for you.

## Usage

```
  (def all-tabs-data (get-all-tabs
                       "LudoTheHUN"   ;;you account name
                       "1231231231231231231231313"   ;;POESESSID, get it from your browser cookies , a 32 char string
                       "Breach"
                       ))

```

  Your item data scraped from the https://www.pathofexile.com/ site...
  
  TODO: solve for optimal (armour/ring/belt) items sets given some constraints
  
  

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
