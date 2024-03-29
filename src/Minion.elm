module Minion exposing
    ( Action
    , Minion
    , Skill(..)
    , SkillAction
    , deriveLevel
    , getActions
    , grantExperience
    , new
    , skillActionString
    , tick
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
    = Auto Action
    | Manual Action


skillActionString : SkillAction -> String
skillActionString action =
    case action of
        Auto autoAction ->
            "Auto " ++ actionString autoAction

        Manual manualAction ->
            "Manual " ++ actionString manualAction


type Action
    = PlantTree
    | CutTree
    | ConstructBuilding
    | ResetLocation


actionString : Action -> String
actionString action =
    case action of
        PlantTree ->
            "PlantTree"

        CutTree ->
            "CutTree"

        ConstructBuilding ->
            "ConstructBuilding"

        ResetLocation ->
            "ResetLocation"


type MinionState
    = Idle
    | Recovering Float
    | Action Action Float


type alias Minion =
    { icon : Char
    , skill : Skill
    , experience : Int
    , state : MinionState
    }


new : Char -> Skill -> Minion
new icon skill =
    Minion icon skill 0 Idle


deriveLevel : Minion -> Int
deriveLevel minion =
    minion.experience // 100 |> max 1


grantExperience : Int -> Minion -> Minion
grantExperience xp minion =
    { minion | experience = minion.experience + max 0 xp }


tick : Float -> Minion -> Minion
tick dt minion =
    case minion.state of
        Idle ->
            minion

        Recovering time ->
            { minion | state = Recovering (max 0 (time - dt)) }

        Action action time ->
            { minion | state = Action action (max 0 (time - dt)) }


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
