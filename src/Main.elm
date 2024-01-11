module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Inventory exposing (Inventory, Slot)
import Location exposing (Location)



-- MODEL


type alias Model =
    { locations : List (Location Int)
    , inventory : Inventory Int
    , selection : Maybe ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Location "Forest" (Inventory.new 3)
        , Location "Forest 2" (Inventory.new 3)
        , Location "Forest 3" (Inventory.new 3)
        , Location "Forest 4" (Inventory.new 3)
        , Location "Forest 5" (Inventory.new 3)
        , Location "Forest 6" (Inventory.new 3)
        , Location "Forest 2" (Inventory.new 3)
        , Location "Forest 3" (Inventory.new 3)
        , Location "Forest 4" (Inventory.new 3)
        , Location "Forest 5" (Inventory.new 3)
        , Location "Forest 6" (Inventory.new 3)
        ]
        (Inventory.new 4
            |> Inventory.insert 0 10
            |> Inventory.insert 2 20
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
                Just ( _, selectedIndex ) ->
                    ( { model
                        | inventory =
                            model.inventory
                                |> Inventory.switch selectedIndex clickedIndex
                        , selection = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    case slot of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( { model | selection = Just ( locationIndex, clickedIndex ) }, Cmd.none )



-- VIEW


viewLocation : Maybe ( Int, Int ) -> Int -> Location Int -> Html Msg
viewLocation selection index location =
    Html.div [ Html.Attributes.class "location" ]
        [ Html.h3 [] [ Html.text location.name ]
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
                Nothing ->
                    True

                Just _ ->
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
                Just item ->
                    Html.text (String.fromInt item)

                Nothing ->
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
        [ Html.div [ Html.Attributes.class "locations" ] (List.indexedMap (viewLocation model.selection) model.locations)
        , viewInventory model.selection -1 model.inventory
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
