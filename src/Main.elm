module Main exposing (Model, Msg, main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Inventory exposing (Inventory, Slot(..))
import Location exposing (Location)



-- MODEL


type alias Model =
    { locations : Dict Int (Location Int)
    , selection : Maybe ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( 0, Location "Forest" (Inventory.new 3 |> Inventory.insert 0 10) )
            , ( 1, Location "Forest 2" (Inventory.new 3) )
            , ( 2, Location "Forest 3" (Inventory.new 3) )
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
        ClickedSlot ( locationIndex, clickedIndex ) slot ->
            case model.selection of
                Just ( selectedLocation, selectedIndex ) ->
                    if locationIndex == selectedLocation then
                        ( { model
                            | locations =
                                model.locations
                                    |> Dict.update locationIndex (\ml -> Maybe.map (\l -> { l | inventory = Inventory.switch clickedIndex selectedIndex l.inventory }) ml)
                            , selection = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        -- handle inter-inventory switch
                        ( { model | selection = Nothing }, Cmd.none )

                Nothing ->
                    case slot of
                        Empty ->
                            ( model, Cmd.none )

                        Item _ ->
                            ( { model | selection = Just ( locationIndex, clickedIndex ) }, Cmd.none )



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
