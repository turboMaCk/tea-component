module Main exposing (..)

{-| This module glues all other examples into one Html.Program

This is used mostly for testing purposes. If this file compiles
than all examples are up to data with recent API.

It's really discutable if something like this is usefull in practice.
Generally I would say it's not in most cases. This is really like rendering multiple TEA
applications into single html using multiple `embed`s.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Glue exposing (Glue)


-- Sub Modules

import Counter.Main as Counter
import Bubbling.Main as Bubbling
import Subscriptions.Main as Subscriptions


counter : Glue Model Counter.Model Msg Counter.Msg Counter.Msg
counter =
    Glue.simple
        { msg = CounterMsg
        , get = .counterModel
        , set = \sm m -> { m | counterModel = sm }
        , init = \_ -> Counter.init
        , update = Counter.update
        , subscriptions = Counter.subscriptions
        }


bubbling : Glue Model Bubbling.Model Msg Bubbling.Msg Bubbling.Msg
bubbling =
    Glue.simple
        { msg = BubblingMsg
        , get = .bubblingModel
        , set = \sm m -> { m | bubblingModel = sm }
        , init = \_ -> Bubbling.init
        , update = Bubbling.update
        , subscriptions = Bubbling.subscriptions
        }


subscriptions : Glue Model Subscriptions.Model Msg Subscriptions.Msg Subscriptions.Msg
subscriptions =
    Glue.simple
        { msg = SubscriptionsMsg
        , get = .subscriptionsModel
        , set = \sm m -> { m | subscriptionsModel = sm }
        , init = \_ -> Subscriptions.init
        , update = Subscriptions.update
        , subscriptions = Subscriptions.subscriptions
        }



-- Model


type SelectedCounter
    = First
    | Second


type alias Model =
    { selectedCounter : SelectedCounter
    , counterModel : Counter.Model
    , bubblingModel : Bubbling.Model
    , subscriptionsModel : Subscriptions.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model First, Cmd.none )
        |> Glue.init counter
        |> Glue.init bubbling
        |> Glue.init subscriptions



-- Update


type Msg
    = CounterMsg Counter.Msg
    | BubblingMsg Bubbling.Msg
    | SubscriptionsMsg Subscriptions.Msg
    | SelectCounter SelectedCounter
    | IncrementSelected


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixl 0 =>


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( model, Cmd.none )
                |> Glue.update counter counterMsg

        BubblingMsg bubblingMsg ->
            ( model, Cmd.none )
                |> Glue.update bubbling bubblingMsg

        SubscriptionsMsg subscriptionsMsg ->
            ( model, Cmd.none )
                |> Glue.update subscriptions subscriptionsMsg

        SelectCounter selected ->
            { model | selectedCounter = selected } => Cmd.none

        IncrementSelected ->
            case model.selectedCounter of
                First ->
                    model
                        |> Glue.updateWith counter Counter.increment
                        => Cmd.none

                Second ->
                    ( model, Cmd.none )
                        |> Glue.trigger bubbling Bubbling.triggerIncrement


countersControlView : Model -> Html Msg
countersControlView model =
    let
        radiosName =
            name "selected-input"
    in
        div []
            [ text "Update Selected counter:"
            , div []
                [ label []
                    [ text "First counter"
                    , input
                        [ type_ "radio"
                        , radiosName
                        , checked <| model.selectedCounter == First
                        , onCheck <| \_ -> SelectCounter First
                        ]
                        []
                    ]
                , br [] []
                , label []
                    [ text "Second counter"
                    , input
                        [ type_ "radio"
                        , radiosName
                        , checked <| model.selectedCounter == Second
                        , onCheck <| \_ -> SelectCounter Second
                        ]
                        []
                    ]
                , div []
                    [ button [ onClick IncrementSelected ]
                        [ text "increment selected" ]
                    ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        line =
            hr [ style [ ( "border", "1px solid rgb(209, 230, 236)" ) ] ]
                []
    in
        div
            [ style
                [ ( "background", "rgb(209, 230, 236)" )
                , ( "position", "absolute" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "padding", "15% 0" )
                , ( "font-family", "Helvetica, Arial, sans-serif" )
                ]
            ]
            [ main_
                [ style
                    [ ( "text-align", "center" )
                    , ( "width", "300px" )
                    , ( "line-height", "2em" )
                    , ( "margin", "0 auto" )
                    , ( "padding", "20px 12px" )
                    , ( "background", "white" )
                    , ( "box-shadow", "0px 2px 4px rgba(0,0,0,.2)" )
                    , ( "border-radius", "3px" )
                    ]
                ]
                [ Glue.view counter Counter.view model
                , line
                , Glue.view bubbling Bubbling.view model
                , line
                , countersControlView model
                , line
                , Glue.view subscriptions Subscriptions.view model
                ]
            ]



-- Subscriptions


subscriptions_ : Model -> Sub Msg
subscriptions_ =
    (\model -> Sub.none)
        |> Glue.subscriptions counter
        |> Glue.subscriptions bubbling
        |> Glue.subscriptions subscriptions



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions_
        }
