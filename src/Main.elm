module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Inventory exposing (Inventory, Slot)



-- MODEL


type alias Model =
    { inventory : Inventory Int
    , selection : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Inventory.new 4 |> Inventory.insert 0 10) Nothing, Cmd.none )



-- UPDATE


type Msg
    = ClickedSlot Int (Slot Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSlot index (Just _) ->
            ( { model | selection = Just index }, Cmd.none )

        ClickedSlot _ Nothing ->
            ( model, Cmd.none )



-- VIEW


viewSlot : Maybe Int -> ( Int, Slot Int ) -> Html Msg
viewSlot selection ( index, slot ) =
    let
        isSelected : Bool
        isSelected =
            selection
                |> Maybe.map ((==) index)
                |> Maybe.withDefault False
    in
    Html.button
        [ Html.Events.onClick (ClickedSlot index slot)
        , Html.Attributes.classList
            [ ( "slot", True )
            , ( "selected", isSelected )
            ]
        ]
        [ Html.p [] [ Html.text (String.fromInt index) ]
        , Html.p []
            [ case slot of
                Just item ->
                    Html.text (String.fromInt item)

                Nothing ->
                    Html.text "empty"
            ]
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div
            [ Html.Attributes.class "inventory"
            ]
            (model.inventory
                |> Inventory.toList
                |> List.map (viewSlot model.selection)
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
