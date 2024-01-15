module Location exposing (Location, LocationState(..), hasSkill, tick)

import Inventory exposing (Inventory, Slot(..))
import Minion exposing (Minion, Skill)


type LocationState
    = Forest ( Float, Float ) Int
    | None


type alias Location =
    { state : LocationState
    , inventory : Inventory Minion
    }


tick : Float -> Location -> Location
tick dt location =
    case location.state of
        Forest ( cd, maxCd ) trees ->
            let
                newCd =
                    (cd + dt) |> min maxCd
            in
            if newCd >= maxCd then
                { location | state = Forest ( 0, maxCd ) (trees + 1) }

            else
                { location | state = Forest ( newCd, maxCd ) trees }

        None ->
            location


hasSkill : Skill -> Int -> Location -> Bool
hasSkill skill level location =
    Inventory.any (Minion.hasLevel skill level) location.inventory
