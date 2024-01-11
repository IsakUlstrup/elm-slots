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
    ( Model
        (Inventory.new 4
            |> Inventory.insert 0 10
            |> Inventory.insert 2 20
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedSlot Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSlot clickedIndex ->
            case model.selection of
                Just selectedIndex ->
                    ( { model
                        | inventory =
                            model.inventory
                                |> Inventory.switch selectedIndex clickedIndex
                        , selection = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | selection = Just clickedIndex }, Cmd.none )



-- VIEW


viewSlot : Maybe Int -> ( Int, Slot Int ) -> Html Msg
viewSlot selection ( index, slot ) =
    let
        isSelected : Bool
        isSelected =
            selection
                |> Maybe.map ((==) index)
                |> Maybe.withDefault False

        isEmpty : Bool
        isEmpty =
            case slot of
                Nothing ->
                    True

                Just _ ->
                    False
    in
    Html.button
        [ Html.Events.onClick (ClickedSlot index)
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
