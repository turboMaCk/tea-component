module Bubbling.Main exposing (Model, Msg, init, update, view, subscriptions)

{-| This is example of child to parent communication using Cmd bubbling.

This Example works as demonstration of such a communication and do not really
reflect real world use-case of this practice. Clearly if parent component is interested
in model of sub component (Even/Odd is really tightly related to child model)
it should really be part of its Model and passed to child rather than other way around.
-}

import Html exposing (Html)


-- Library

import Glue exposing (Glue)


-- Submodules

import Bubbling.Counter as Counter


counter : Glue Model Counter.Model Msg Counter.Msg
counter =
    Glue.poly
        { get = .counter
        , set = \subModel model -> { model | counter = subModel }
        , init = Counter.init Even
        , update = Counter.update Even
        , subscriptions = \_ -> Sub.none
        }
        |> Glue.withView (\model -> Counter.view CounterMsg model.counter)



-- Main


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Sub.none)
        |> Glue.subscriptions counter


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
    { even : Bool
    , counter : Counter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model False, Cmd.none )
        |> Glue.init counter



-- Update


type Msg
    = CounterMsg Counter.Msg
    | Even


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( { model | even = False }, Cmd.none )
                |> Glue.update counter counterMsg

        Even ->
            ( { model | even = True }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Glue.view counter model
        , if model.even then
            Html.text "is even"
          else
            Html.text "is odd"
        ]
