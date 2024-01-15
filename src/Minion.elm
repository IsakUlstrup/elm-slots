module Minion exposing (Minion, Skill(..), new)

import CustomDict exposing (Dict)


type Skill
    = Debug


type alias Minion =
    { icon : Char
    , skills : Dict Skill Int
    }


new : Char -> List ( Skill, Int ) -> Minion
new icon skills =
    Minion icon (CustomDict.fromList skills)
