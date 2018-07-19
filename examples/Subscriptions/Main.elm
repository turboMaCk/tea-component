module Subscriptions.Main exposing (Model, Msg, init, update, view, subscriptions)

{-| This is example of slightly bit more complex management of subscriptions
between parent and child.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onCheck)
import Mouse exposing (Position)


-- Library

import Glue exposing (Glue)


-- Submodules

import Subscriptions.Moves as Moves


moves : Glue Model Moves.Model Msg Moves.Msg Moves.Msg
moves =
    Glue.simple
        { msg = MovesMsg
        , get = .moves
        , set = \subModel model -> { model | moves = subModel }
        , init = \_ -> Moves.init
        , update = Moves.update
        , subscriptions = Moves.subscriptions
        }



-- Main


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Mouse.clicks Clicked)
        |> Glue.subscriptionsWhen .movesOn moves


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { clicks : Int
    , movesOn : Bool
    , moves : Moves.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 True, Cmd.none )
        |> Glue.init moves



-- Update


type Msg
    = Clicked Position
    | MovesMsg Moves.Msg
    | ToggleMoves Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MovesMsg movesMsg ->
            ( model, Cmd.none )
                |> Glue.update moves movesMsg

        Clicked _ ->
            ( { model | clicks = model.clicks + 1 }, Cmd.none )

        ToggleMoves bool ->
            ( { model | movesOn = bool }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text <| "Clicks: " ++ (toString model.clicks)
        , Html.label [ HtmlA.style [ ( "display", "block" ) ] ]
            [ Html.text "subscribe to mouse moves"
            , Html.input
                [ onCheck ToggleMoves
                , HtmlA.type_ "checkbox"
                , HtmlA.checked model.movesOn
                ]
                []
            ]
        , Html.div []
            [ Html.text "Position: "
            , Glue.view moves Moves.view model
            ]
        ]
