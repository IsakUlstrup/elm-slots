module Location exposing (Location, LocationState(..), minions, reset, tick)

import Inventory exposing (Inventory, Slot(..))
import Minion exposing (Minion)


type LocationState
    = Sapling ( Float, Float )
    | Tree ( Float, Float )
    | InspectMinion
    | None


type alias Location =
    { state : LocationState
    , inventory : Inventory Minion
    }


tickState : Float -> Location -> Location
tickState dt location =
    case location.state of
        Sapling ( cd, maxCd ) ->
            { location | state = Sapling ( (cd + dt) |> min maxCd, maxCd ) }

        Tree _ ->
            location

        InspectMinion ->
            location

        None ->
            location


tickMinions : Float -> Location -> Location
tickMinions dt location =
    { location | inventory = Inventory.map (Minion.tick dt) location.inventory }


tick : Float -> Location -> Location
tick dt location =
    location
        |> tickState dt
        |> tickMinions dt


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
