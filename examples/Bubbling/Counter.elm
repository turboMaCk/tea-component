module Bubbling.Counter exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Html.Events
import Task



-- Model


type alias Model =
    Int


init : (Int -> msg) -> ( Model, Cmd msg )
init msg =
    let
        model =
            0
    in
    ( model, notify msg model )



-- update


type Msg
    = Increment
    | Decrement


notify : (Int -> msg) -> Int -> Cmd msg
notify msg count =
    Task.perform identity <| Task.succeed <| msg count


update : (Int -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update parentMsg msg model =
    let
        newModel =
            case msg of
                Increment ->
                    model + 1

                Decrement ->
                    model - 1
    in
    ( newModel, notify parentMsg newModel )



-- View


view : (Msg -> msg) -> Model -> Html msg
view msg model =
    Html.map msg <|
        Html.div []
            [ Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
            , Html.text <| String.fromInt model
            , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
            ]
