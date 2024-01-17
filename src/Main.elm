module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Minions as Minions
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Inventory exposing (Inventory, Slot(..))
import Location exposing (Location)
import Minion exposing (Minion)


switchSlots : ( Int, Int ) -> ( Int, Int ) -> Dict Int Location -> Dict Int Location
switchSlots ( fromLocation, fromSlot ) ( toLocation, toSlot ) locations =
    let
        getSlot loc slot =
            Dict.get loc locations
                |> Maybe.map (.inventory >> Inventory.get slot)

        maybeToSlot : Maybe a -> Slot a
        maybeToSlot m =
            case m of
                Just a ->
                    Item a

                Nothing ->
                    Empty

        removeInsert : Int -> Maybe Minion -> Location -> Location
        removeInsert f t l =
            { l
                | inventory =
                    l.inventory
                        |> Inventory.remove f
                        |> Inventory.insert f (maybeToSlot t)
            }
    in
    case ( getSlot fromLocation fromSlot, getSlot toLocation toSlot ) of
        ( Just from, Just to ) ->
            locations
                |> Dict.update fromLocation (Maybe.map (removeInsert fromSlot to))
                |> Dict.update toLocation (Maybe.map (removeInsert toSlot from))

        _ ->
            locations



-- MODEL


type alias Model =
    { locations : Dict Int Location
    , selection : Maybe ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( -1
              , Location
                    Location.None
                    (Inventory.new 6
                        |> Inventory.insert 0 (Item Minions.debug)
                        |> Inventory.insert 1 (Item Minions.builder)
                    )
              )
            , ( 0, Location Location.None (Inventory.new 3) )
            , ( 1, Location Location.None (Inventory.new 3) )
            , ( 2, Location (Location.Tree ( 0, 50000 )) (Inventory.new 3) )
            ]
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedSlot ( Int, Int ) (Slot Minion)
    | ClickedResetLocation Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | locations =
                    model.locations
                        |> Dict.map (\_ location -> Location.tick dt location)
              }
            , Cmd.none
            )

        ClickedSlot clickedLocation slot ->
            case model.selection of
                Just selectedLocation ->
                    ( { model
                        | locations =
                            model.locations
                                |> switchSlots selectedLocation clickedLocation
                        , selection = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    case slot of
                        Empty ->
                            ( model
                            , Cmd.none
                            )

                        Item _ ->
                            ( { model | selection = Just clickedLocation }
                            , Cmd.none
                            )

        ClickedResetLocation index ->
            ( { model
                | locations =
                    model.locations
                        |> Dict.update index (\l -> Maybe.map Location.reset l)
              }
            , Cmd.none
            )



-- VIEW


viewLocation : Maybe ( Int, Int ) -> ( Int, Location ) -> Html Msg
viewLocation selection ( index, location ) =
    let
        viewState : Html msg
        viewState =
            Html.div [ Html.Attributes.class "location-state" ]
                (case location.state of
                    Location.None ->
                        []

                    Location.Tree ( cd, maxCd ) ->
                        [ Html.h1 []
                            [ Html.text
                                (if cd == maxCd then
                                    "🌲"

                                 else
                                    "🌱"
                                )
                            ]
                        , Html.progress
                            [ Html.Attributes.value (String.fromFloat cd)
                            , Html.Attributes.max (String.fromFloat maxCd)
                            ]
                            []
                        ]
                )

        viewActions : Html Msg
        viewActions =
            Html.div [ Html.Attributes.class "actions" ]
                (case selection of
                    Just ( selectedLocation, selectedSlot ) ->
                        if selectedLocation == index then
                            case Inventory.get selectedSlot location.inventory of
                                Just m ->
                                    if Minion.hasLevel Minion.Debug 5 m then
                                        [ Html.button [ Html.Events.onClick (ClickedResetLocation index) ] [ Html.text "Reset location" ] ]

                                    else
                                        []

                                Nothing ->
                                    []

                        else
                            []

                    Nothing ->
                        []
                )
    in
    Html.div [ Html.Attributes.class "location" ]
        [ viewState
        , viewActions
        , viewInventory selection index location.inventory
        ]


viewSlot : Int -> Maybe ( Int, Int ) -> ( Int, Slot Minion ) -> Html Msg
viewSlot locationIndex selection ( index, slot ) =
    let
        isSelected : Bool
        isSelected =
            case selection of
                Just ( selectedLocation, selectedSlot ) ->
                    selectedLocation == locationIndex && selectedSlot == index

                Nothing ->
                    False

        isEmpty : Bool
        isEmpty =
            case slot of
                Empty ->
                    True

                Item _ ->
                    False
    in
    Html.button
        [ Html.Events.onClick (ClickedSlot ( locationIndex, index ) slot)
        , Html.Attributes.classList
            [ ( "slot", True )
            , ( "selected", isSelected )
            , ( "empty", isEmpty )
            ]
        ]
        [ -- Html.p [] [ Html.text (String.fromInt index) ]
          Html.p []
            [ case slot of
                Item item ->
                    Html.text (String.fromChar item.icon)

                Empty ->
                    Html.text ""
            ]
        ]


viewInventory : Maybe ( Int, Int ) -> Int -> Inventory Minion -> Html Msg
viewInventory selection index inventory =
    Html.div
        [ Html.Attributes.class "inventory"
        ]
        (inventory
            |> Inventory.toList
            |> List.map (viewSlot index selection)
        )


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "locations" ]
            (model.locations
                |> Dict.filter (\index _ -> index >= 0)
                |> Dict.toList
                |> List.map (viewLocation model.selection)
            )
        , Html.div [ Html.Attributes.class "backpack" ]
            (case Dict.get -1 model.locations of
                Just l ->
                    [ viewLocation model.selection ( -1, l ) ]

                Nothing ->
                    []
            )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
