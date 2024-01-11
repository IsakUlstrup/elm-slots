module Inventory exposing (Inventory, Slot, new, toList)

import Dict exposing (Dict)


{-| Slot type, represents value at each inventory slot
-}
type alias Slot =
    Maybe Int


{-| Main Inventory type. It's an opaque dict with Int keys and Slot values
-}
type Inventory
    = Inventory (Dict Int Slot)


{-|

    Create new empty inventory of given size

-}
new : Int -> Inventory
new size =
    size
        |> clamp 0 100
        |> (\i -> i - 1)
        |> List.range 0
        |> List.map (\i -> ( i, Nothing ))
        |> Dict.fromList
        |> Inventory


{-| Get Inventory slots as a List of Int Slot Tuples
-}
toList : Inventory -> List ( Int, Slot )
toList (Inventory inventory) =
    Dict.toList inventory
