module Inventory exposing
    ( Inventory
    , Slot(..)
    , get
    , insert
    , new
    , remove
    , switch
    , toList
    )

import Dict exposing (Dict)


{-| Slot type, represents value at each inventory slot
-}
type Slot a
    = Item a
    | Empty


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
        |> List.map (\i -> ( i, Empty ))
        |> Dict.fromList
        |> Inventory


{-| Insert item into slot at index, returns unchanged inventory if index is out of range or occupied
-}
insert : Int -> a -> Inventory a -> Inventory a
insert index item (Inventory inventory) =
    case Dict.get index inventory of
        Just (Item _) ->
            -- slot is occupied, return unchanged
            Inventory inventory

        Just Empty ->
            -- slot exists and is empty, insert
            Inventory
                (inventory
                    |> Dict.update index (always (Just (Item item)))
                )

        Nothing ->
            -- slot does not exist, return unchanged
            Inventory inventory


{-| Remove item at index
-}
remove : Int -> Inventory a -> Inventory a
remove index (Inventory inventory) =
    if Dict.member index inventory then
        Inventory (inventory |> Dict.update index (always (Just Empty)))

    else
        Inventory inventory


{-| Get item from slot at index
-}
get : Int -> Inventory a -> Maybe a
get index (Inventory inventory) =
    Dict.get index inventory
        |> (\mi ->
                case mi of
                    Just (Item item) ->
                        Just item

                    _ ->
                        Nothing
           )


{-| Switch items at fromIndex and toIndex

Returns unchanged inventory if either index does not exist

-}
switch : Int -> Int -> Inventory a -> Inventory a
switch fromIndex toIndex (Inventory inventory) =
    case ( Dict.get fromIndex inventory, Dict.get toIndex inventory ) of
        ( Just from, Just to ) ->
            Inventory (inventory |> Dict.insert fromIndex to |> Dict.insert toIndex from)

        _ ->
            Inventory inventory


{-| Get Inventory slots as a List of Int Slot Tuples
-}
toList : Inventory a -> List ( Int, Slot a )
toList (Inventory inventory) =
    Dict.toList inventory
