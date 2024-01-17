module Location exposing (Location, LocationState(..), minions, reset, tick)

import Inventory exposing (Inventory, Slot(..))
import Minion exposing (Minion)


type LocationState
    = Tree ( Float, Float )
    | InspectMinion
    | None


type alias Location =
    { state : LocationState
    , inventory : Inventory Minion
    }


tick : Float -> Location -> Location
tick dt location =
    case location.state of
        Tree ( cd, maxCd ) ->
            { location | state = Tree ( (cd + dt) |> min maxCd, maxCd ) }

        InspectMinion ->
            location

        None ->
            location


minions : Location -> List Minion
minions location =
    location.inventory
        |> Inventory.toList
        |> List.map Tuple.second
        |> List.filterMap
            (\slot ->
                case slot of
                    Item item ->
                        Just item

                    Empty ->
                        Nothing
            )


reset : Location -> Location
reset location =
    { location | state = None }
