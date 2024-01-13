module Main exposing (Model, Msg, main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Inventory exposing (Inventory, Slot(..))
import Location exposing (Location)


switchSlots : ( Int, Int ) -> ( Int, Int ) -> Dict Int (Location Int) -> Dict Int (Location Int)
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

        removeInsert : Int -> Maybe a -> Location a -> Location a
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
    { locations : Dict Int (Location Int)
    , selection : Maybe ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( 0, Location "Forest" (Inventory.new 3 |> Inventory.insert 0 (Item 10)) )
            , ( 1, Location "Forest 2" (Inventory.new 3) )
            , ( 2, Location "Forest 3" (Inventory.new 3 |> Inventory.insert 0 (Item 3)) )
            ]
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedSlot ( Int, Int ) (Slot Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                            ( model, Cmd.none )

                        Item _ ->
                            ( { model | selection = Just clickedLocation }, Cmd.none )



-- VIEW


viewLocation : Maybe ( Int, Int ) -> ( Int, Location Int ) -> Html Msg
viewLocation selection ( index, location ) =
    Html.div [ Html.Attributes.class "location" ]
        [ Html.h1 [] [ Html.text location.name ]
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
                |> Dict.toList
                |> List.map (viewLocation model.selection)
            )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
