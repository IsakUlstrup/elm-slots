module Inventory exposing (Inventory, Slot, insert, new, toList)

import Dict exposing (Dict)


{-| Slot type, represents value at each inventory slot
-}
type alias Slot a =
    Maybe a


{-| Main Inventory type. It's an opaque dict with Int keys and Slot values
-}
type Inventory a
    = Inventory (Dict Int (Slot a))


{-|

    Create new empty inventory of given size

-}
new : Int -> Inventory a
new size =
    size
        |> clamp 0 100
        |> (\i -> i - 1)
        |> List.range 0
        |> List.map (\i -> ( i, Nothing ))
        |> Dict.fromList
        |> Inventory


{-| Insert item into slot at index, returns unchanged inventory if index is out of range or occupied
-}
insert : Int -> a -> Inventory a -> Inventory a
insert index item (Inventory inventory) =
    case Dict.get index inventory of
        Just (Just _) ->
            -- slot is occupied, return unchanged
            Inventory inventory

        Just Nothing ->
            -- slot exists and is empty, insert
            Inventory
                (inventory
                    |> Dict.update index (always (Just (Just item)))
                )

        Nothing ->
            -- slot does not exist, return unchanged
            Inventory inventory


{-| Get Inventory slots as a List of Int Slot Tuples
-}
toList : Inventory a -> List ( Int, Slot a )
toList (Inventory inventory) =
    Dict.toList inventory
