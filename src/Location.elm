module Location exposing (Location, LocationState(..), hasSkill, reset, tick)

import Inventory exposing (Inventory, Slot(..))
import Minion exposing (Minion, Skill)


type LocationState
    = Tree ( Float, Float )
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

        None ->
            location


reset : Location -> Location
reset location =
    case location.state of
        None ->
            location

        Tree _ ->
            { location | state = None }


hasSkill : Skill -> Int -> Location -> Bool
hasSkill skill level location =
    Inventory.any (Minion.hasLevel skill level) location.inventory
