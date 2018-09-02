module Bubbling.Main exposing (Model, Msg, init, subscriptions, triggerIncrement, update, view)

{-| This is example of child to parent communication using Cmd bubbling.

This Example works as demonstration of such a communication and do not really
reflect real world use-case of this practice. Clearly if parent component is interested
in model of sub component (Even/Odd is really tightly related to child model)
it should really be part of its Model and passed to child rather than other way around.

-}

import Browser
import Bubbling.Counter as Counter
import Glue exposing (Glue)
import Html exposing (Html)
import Task


counter : Glue Model Counter.Model Msg Counter.Msg Msg
counter =
    Glue.poly
        { get = .counter
        , set = \subModel model -> { model | counter = subModel }
        , init = \_ -> Counter.init CountChanged
        , update = Counter.update CountChanged
        , subscriptions = \_ -> Sub.none
        }


triggerIncrement : Model -> Cmd Msg
triggerIncrement _ =
    Cmd.none



-- Main


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Sub.none)
        |> Glue.subscriptions counter


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
    { max : Int
    , counter : Counter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0, Cmd.none )
        |> Glue.init counter



-- Update


type Msg
    = CounterMsg Counter.Msg
    | CountChanged Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( model, Cmd.none )
                |> Glue.update counter counterMsg

        CountChanged num ->
            if num > model.max then
                ( { model | max = num }, Cmd.none )
            else
                ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Glue.view counter (Counter.view CounterMsg) model
        , Html.text <| "Max historic value: " ++ String.fromInt model.max
        ]
