module Location exposing (Location, LocationState(..), tick)

import Inventory exposing (Inventory, Slot(..))


type LocationState
    = Forest ( Float, Float ) Int
    | None


type alias Location =
    { name : String
    , state : LocationState
    , inventory : Inventory Int
    }


tick : Float -> Location -> Location
tick dt location =
    case location.state of
        Forest ( cd, maxCd ) trees ->
            let
                newCd =
                    (cd + (dt * toFloat sumInv)) |> min maxCd

                sumInv =
                    location.inventory
                        |> Inventory.toList
                        |> List.map Tuple.second
                        |> List.filterMap
                            (\slot ->
                                case slot of
                                    Item i ->
                                        Just i

                                    Empty ->
                                        Nothing
                            )
                        |> List.sum
            in
            if newCd >= maxCd then
                { location | state = Forest ( 0, maxCd ) (trees + 1) }

            else
                { location | state = Forest ( newCd, maxCd ) trees }

        None ->
            location
