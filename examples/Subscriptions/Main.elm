module Subscriptions.Main exposing (Model, Msg, init, subscriptions, update, view)

{-| This is example of slightly bit more complex management of subscriptions
between parent and child.
-}

import Browser
import Browser.Events
import Glue exposing (Glue)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onCheck)
import Json.Decode as Decode
import Subscriptions.Moves as Moves exposing (Position)


moves : Glue Model Moves.Model Msg Moves.Msg
moves =
    Glue.glue
        { msg = MovesMsg
        , get = .moves
        , set = \subModel model -> { model | moves = subModel }
        }



-- Main


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Browser.Events.onClick <| Decode.map Clicked Moves.positionDecoder)
        |> Glue.subscriptionsWhen .movesOn moves Moves.subscriptions


main : Program () Model Msg
main =
    Browser.element
        { init = always init
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
        |> Glue.init moves Moves.init



-- Update


type Msg
    = Clicked { x : Int, y : Int }
    | MovesMsg Moves.Msg
    | ToggleMoves Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MovesMsg movesMsg ->
            ( model, Cmd.none )
                |> Glue.update moves Moves.update movesMsg

        Clicked _ ->
            ( { model | clicks = model.clicks + 1 }, Cmd.none )

        ToggleMoves bool ->
            ( { model | movesOn = bool }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text <| "Clicks: " ++ String.fromInt model.clicks
        , Html.label [ HtmlA.style "display" "block" ]
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
