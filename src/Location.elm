module Location exposing (Location)

import Inventory exposing (Inventory)


type alias Location a =
    { name : String
    , inventory : Inventory a
    }
