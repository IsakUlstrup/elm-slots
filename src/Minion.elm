module Minion exposing (Minion, Skill(..), hasLevel, new)

import CustomDict exposing (Dict)


type Skill
    = Debug
    | Construction


type alias Minion =
    { icon : Char
    , skills : Dict Skill Int
    }


new : Char -> List ( Skill, Int ) -> Minion
new icon skills =
    Minion icon (CustomDict.fromList skills)


hasLevel : Skill -> Int -> Minion -> Bool
hasLevel skill level minion =
    case CustomDict.get skill minion.skills of
        Just s ->
            s >= level

        Nothing ->
            False
