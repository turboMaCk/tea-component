module Counter.Counter exposing (Model, Msg, init, update, view)

import Html exposing (Html)
import Html.Events



-- Model


type alias Model =
    Int


init : Model
init =
    0



-- update


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
        , Html.text <| String.fromInt model
        , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
        ]
