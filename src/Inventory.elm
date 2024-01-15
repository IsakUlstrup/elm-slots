module Inventory exposing
    ( Inventory
    , Slot(..)
    , any
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
insert : Int -> Slot a -> Inventory a -> Inventory a
insert index item (Inventory inventory) =
    case Dict.get index inventory of
        Just (Item _) ->
            -- slot is occupied, return unchanged
            Inventory inventory

        Just Empty ->
            -- slot exists and is empty, insert
            Inventory
                (inventory
                    |> Dict.update index (always (Just item))
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


{-| Check if any occupied slot passes provided predicate
-}
any : (a -> Bool) -> Inventory a -> Bool
any pred (Inventory inventory) =
    Dict.toList inventory
        |> List.map Tuple.second
        |> List.filterMap
            (\slot ->
                case slot of
                    Item i ->
                        Just i

                    Empty ->
                        Nothing
            )
        |> List.any pred


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
