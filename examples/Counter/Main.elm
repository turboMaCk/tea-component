module Counter.Main exposing (Model, Msg, increment, init, subscriptions, update, view)

{-| This show how you can glue really simple statefull submodule.

This example is quite artificial demostration of what is possible to do with `Glue`.
In real world it probably doesn't make sense to use similar approach
for something as simple as counter and text which are interested in same action.

-}

import Browser
import Counter.Counter as Counter
import Glue exposing (Glue)
import Html exposing (Html)


counter : Glue Model Counter.Model Msg Counter.Msg Counter.Msg
counter =
    Glue.simple
        { msg = CounterMsg
        , get = .counter
        , set = \subModel model -> { model | counter = subModel }
        , init = \_ -> Counter.init
        , update = Counter.update
        , subscriptions = \_ -> Sub.none
        }


increment : Model -> Model
increment model =
    { model
        | message = "Counter changed from outside!"
        , counter = model.counter + 1
    }



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
    { message : String
    , counter : Counter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model "Let's change the counter!", Cmd.none )
        |> Glue.init counter



-- Update


type Msg
    = CounterMsg Counter.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( { model | message = "Counter has changed!" }, Cmd.none )
                |> Glue.update counter counterMsg



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text model.message
        , Glue.view counter Counter.view model
        ]
