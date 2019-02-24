module Counter.Counter exposing (Model, Msg, init, update, view)

import Html exposing (Html)
import Html.Events



-- Model


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- update


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Increment ->
                    model + 1

                Decrement ->
                    model - 1
    in
    ( newModel, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
        , Html.text <| String.fromInt model
        , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
        ]
