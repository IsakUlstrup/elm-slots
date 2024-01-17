module Minion exposing
    ( AutoAction
    , ManualAction
    , Minion
    , Skill(..)
    , SkillAction
    , actionString
    , deriveLevel
    , getActions
    , grantExperience
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
    = Auto AutoAction
    | Manual ManualAction


actionString : SkillAction -> String
actionString action =
    case action of
        Auto autoAction ->
            "Auto " ++ autoActionString autoAction

        Manual manualAction ->
            "Manual " ++ manualActionString manualAction


type AutoAction
    = PlantTree
    | CutTree


autoActionString : AutoAction -> String
autoActionString action =
    case action of
        PlantTree ->
            "PlantTree"

        CutTree ->
            "CutTree"


type ManualAction
    = ConstructBuilding
    | ResetLocation


manualActionString : ManualAction -> String
manualActionString action =
    case action of
        ConstructBuilding ->
            "ConstructBuilding"

        ResetLocation ->
            "ResetLocation"


type alias Minion =
    { icon : Char
    , skill : Skill
    , experience : Int
    }


new : Char -> Skill -> Minion
new icon skill =
    Minion icon skill 0


deriveLevel : Minion -> Int
deriveLevel minion =
    minion.experience // 100 |> max 1


grantExperience : Int -> Minion -> Minion
grantExperience xp minion =
    { minion | experience = minion.experience + max 0 xp }


filterAvailableAction : Int -> ( SkillAction, Int ) -> Maybe SkillAction
filterAvailableAction level ( action, reqLevel ) =
    if level >= reqLevel then
        Just action

    else
        Nothing


getActions : Minion -> List SkillAction
getActions minion =
    let
        derivedLevel =
            deriveLevel minion
    in
    case minion.skill of
        Debug ->
            [ ( Manual ResetLocation, 10 ) ]
                |> List.filterMap
                    (filterAvailableAction derivedLevel)

        Woodcutting ->
            [ ( Auto CutTree, 1 ) ]
                |> List.filterMap
                    (filterAvailableAction derivedLevel)

        Forestry ->
            [ ( Auto PlantTree, 1 ) ]
                |> List.filterMap
                    (filterAvailableAction derivedLevel)


toString : Minion -> String
toString minion =
    "{icon: '"
        ++ String.fromChar minion.icon
        ++ "', skill: "
        ++ skillToString minion.skill
        ++ ", experience: "
        ++ String.fromInt minion.experience
        ++ ", derived level: "
        ++ String.fromInt (deriveLevel minion)
        ++ "}"
