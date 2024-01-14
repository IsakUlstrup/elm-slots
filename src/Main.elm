module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Inventory exposing (Inventory, Slot(..))
import Location exposing (Location)


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

        removeInsert : Int -> Maybe Int -> Location -> Location
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
              , Location "Home"
                    Location.None
                    (Inventory.new 6
                        |> Inventory.insert 0 (Item 1)
                        |> Inventory.insert 1 (Item 2)
                        |> Inventory.insert 2 (Item 3)
                    )
              )
            , ( 0, Location "Forest 1" (Location.Forest ( 0, 5000 ) 0) (Inventory.new 3) )
            , ( 1, Location "Forest 2" (Location.Forest ( 0, 5000 ) 0) (Inventory.new 3) )
            , ( 2, Location "Forest 3" (Location.Forest ( 0, 5000 ) 0) (Inventory.new 3) )
            ]
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedSlot ( Int, Int ) (Slot Int)


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



-- VIEW


viewLocation : Maybe ( Int, Int ) -> ( Int, Location ) -> Html Msg
viewLocation selection ( index, location ) =
    let
        viewState =
            Html.div [ Html.Attributes.class "location-state" ]
                (case location.state of
                    Location.None ->
                        []

                    Location.Forest ( cd, maxCd ) trees ->
                        [ Html.p [] [ Html.text ("Trees: " ++ String.fromInt trees) ]
                        , Html.progress
                            [ Html.Attributes.value (String.fromFloat cd)
                            , Html.Attributes.max (String.fromFloat maxCd)
                            ]
                            []
                        ]
                )
    in
    Html.div [ Html.Attributes.class "location" ]
        [ Html.h1 [ Html.Attributes.class "location-name" ] [ Html.text location.name ]
        , viewState
        , viewInventory selection index location.inventory
        ]


viewSlot : Int -> Maybe ( Int, Int ) -> ( Int, Slot Int ) -> Html Msg
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
                    Html.text (String.fromInt item)

                Empty ->
                    Html.text ""
            ]
        ]


viewInventory : Maybe ( Int, Int ) -> Int -> Inventory Int -> Html Msg
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
