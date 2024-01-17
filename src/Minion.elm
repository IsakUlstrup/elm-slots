module Minion exposing
    ( Minion
    , Skill(..)
    , SkillAction
    , new
    , toString
    )


type Skill
    = Debug
    | Woodcutting
    | Forestry


skillToString : Skill -> String
skillToString skill =
    case skill of
        Debug ->
            "Debug"

        Woodcutting ->
            "Woodcutting"

        Forestry ->
            "Forestry"


type SkillAction
    = PlantTree
    | CutTree
    | ViewDebugState
    | ResetLocation


type alias Minion =
    { icon : Char
    , skill : Skill
    , experience : Int
    }


new : Char -> Skill -> Minion
new icon skill =
    Minion icon skill 0


toString : Minion -> String
toString minion =
    "{icon: '"
        ++ String.fromChar minion.icon
        ++ "', skill: "
        ++ skillToString minion.skill
        ++ ", experience: "
        ++ String.fromInt minion.experience
        ++ "}"
