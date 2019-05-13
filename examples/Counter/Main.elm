module Counter.Main exposing (Model, Msg, increment, init, update, view)

{-| This show how you can glue really simple stateful submodule.

This example is quite artificial demonstration of what is possible to do with `Glue`.
In real world it probably doesn't make sense to use similar approach
for something as simple as counter and text which are interested in same action.

-}

import Browser
import Counter.Counter as Counter
import Glue exposing (Glue)
import Html exposing (Html)


counter : Glue Model Counter.Model Never Never
counter =
    Glue.simple
        { get = .counter
        , set = \subModel model -> { model | counter = subModel }
        }


increment : Model -> Model
increment model =
    { model
        | message = "Counter changed from outside!"
        , counter = model.counter + 1
    }



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { message : String
    , counter : Counter.Model
    }


init : Model
init =
    Model "Let's change the counter!"
        |> Glue.initModel counter Counter.init



-- Update


type Msg
    = CounterMsg Counter.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        CounterMsg counterMsg ->
            { model | message = "Counter has changed!" }
                |> Glue.updateModel counter Counter.update counterMsg



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text model.message
        , Glue.viewSimple counter Counter.view CounterMsg model
        ]
