module Glue exposing
    ( Glue
    , glue, poly
    , init
    , update, updateModel, updateWithTrigger, trigger
    , subscriptions, subscriptionsWhen
    , view
    , map
    )

{-| Composing Elm applications from smaller isolated parts (modules).
You can think about this as about lightweight abstraction built around `(model, Cmd msg)` or
`(model, Sub msg)` pairs repetition of in code for composing `init` `update` `view` and `subscribe` using
[`Cmd.map`](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#map),
[`Sub.map`](https://package.elm-lang.org/packages/elm/core/latest/Platform-Sub#map)
and [`Html.map`](https://package.elm-lang.org/packages/elm/html/latest/Html#map).

It's recommended to avoid usage of pattern with statefull modules unless there is clear benefit to choose it.
In cases where one would like to use `Cmd.map` pattern anyway though,
Glue can be used to avoid repetable patterns for mapping the msg types
and updating models.


# Datatype

@docs Glue


# Constructors

@docs glue, poly


# Init

Designed for chaining initialization of child modules
from parent init function.

@docs init


# Updates

There are 4 versions of functions used to work with model
updates and commands between modules each useful in
a different situation.

@docs update, updateModel, updateWithTrigger, trigger


# Subscriptions

@docs subscriptions, subscriptionsWhen


# View

@docs view


# Helpers

@docs map

-}

import Html exposing (Html)


{-| `Glue` describes an interface between parent and child module.

You can create `Glue` with the [`glue`](#glue) or [`poly`](#poly) function constructor.
Every glue layer is parametrized over:

  - `model` is `Model` of parent
  - `subModel` is `Model` of child
  - `msg` is `Msg` of parent
  - `subMsg` is `Msg` of child

-}
type Glue model subModel msg subMsg
    = Glue
        { msg : subMsg -> msg
        , get : model -> subModel
        , set : subModel -> model -> model
        }


{-| Simple [`Glue`](#Glue) constructor.
-}
glue :
    { msg : subMsg -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    }
    -> Glue model subModel msg subMsg
glue rec =
    Glue rec


{-| Sepcialized version of constructor.
Useful when module's api has generic `msg` type
and maps command internally.

This constructor simply aliases `msg` to `identity`
function.

-}
poly :
    { get : model -> subModel
    , set : subModel -> model -> model
    }
    -> Glue model subModel msg msg
poly rec =
    Glue
        { msg = identity
        , get = rec.get
        , set = rec.set
        }



-- Basics


{-| Initialize child module in parent.

    type alias Model =
        { message : String
        , firstCounterModel : Counter.Model
        , secondCounterModel : Counter.Model
        }

    init : ( Model, Cmd msg )
    init =
        ( Model "", Cmd.none )
            |> Glue.init firstCounter Counter.init
            |> Glue.init secondCounter Counter.init

-}
init : Glue model subModel msg subMsg -> ( subModel, Cmd subMsg ) -> ( subModel -> a, Cmd msg ) -> ( a, Cmd msg )
init (Glue { msg }) ( subModel, subCmd ) ( fc, cmd ) =
    ( fc subModel, Cmd.batch [ cmd, Cmd.map msg subCmd ] )


{-| Call submodule update with given message.
Useful for nesting update calls.

    -- Child module
    updateCounter : Counter.Msg -> Counter.Model -> ( Counter.Model, Cmd Counter.Msg )
    updateCounter msg model =
        case msg of
            Increment ->
                ( model + 1, Cmd.none )

    -- Parent module
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            CounterMsg counterMsg ->
                ( { model | message = "Counter has changed" }, Cmd.none )
                    |> Glue.update counter Counter.update counterMsg

-}
update : Glue model subModel msg subMsg -> (a -> subModel -> ( subModel, Cmd subMsg )) -> a -> ( model, Cmd msg ) -> ( model, Cmd msg )
update (Glue rec) fc msg ( model, cmd ) =
    let
        ( subModel, subCmd ) =
            fc msg <| rec.get model
    in
    ( rec.set subModel model, Cmd.batch [ Cmd.map rec.msg subCmd, cmd ] )


{-| Update child module by given function.

    -- Child module
    incrementBy : Int -> Counter.Model -> Counter.Model
    incrementBy num model =
        model + num

    -- Parent module
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            IncrementBy10 ->
                ( Glue.updateModel counter (incrementBy 10) model
                , Cmd.none
                )

-}
updateModel : Glue model subModel msg subMsg -> (subModel -> subModel) -> model -> model
updateModel (Glue rec) fc model =
    rec.set (fc <| rec.get model) model


{-| Trigger Cmd in by child's function

_Commands are async. Therefore trigger doesn't make any update directly.
Use [`updateModel`](#updateModel) over `trigger` when you can._

    -- Child module
    triggerEmit : Counter.Model -> Cmd Counter.Msg
    triggerEmit model ->
        Task.perform identity <| Task.succeed <| Counter.Emit model

    -- Parent module
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            IncrementCounter ->
                ( model, Cmd.none )
                    |> Glue.trigger counter triggerIncrement

-}
trigger : Glue model subModel msg subMsg -> (subModel -> Cmd subMsg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
trigger (Glue rec) fc ( model, cmd ) =
    ( model, Cmd.batch [ Cmd.map rec.msg <| fc <| rec.get model, cmd ] )


{-| Update child module using functin that also produces `Cmd`.

    increment : Counter.Model -> ( Counter.Model, Cmd Counter.Msg )
    increment model =
        ( model + 1, Cmd.none )

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            IncrementCounter ->
                ( model, Cmd.none )
                    |> Glue.updateWithTrigger counter increment

-}
updateWithTrigger : Glue model subModel msg subMsg -> (subModel -> ( subModel, Cmd subMsg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateWithTrigger (Glue rec) fc ( model, cmd ) =
    let
        ( subModel, subCmd ) =
            fc <| rec.get model
    in
    ( rec.set subModel model, Cmd.batch [ Cmd.map rec.msg subCmd, cmd ] )


{-| Subscribe to subscriptions defined in submodule.

    subscriptions : Model -> Sub Msg
    subscriptions =
        (\model -> Mouse.clicks Clicked)
            |> Glue.subscriptions foo Foo.subscriptions
            |> Glue.subscriptions bar Bar.subscriptions

-}
subscriptions : Glue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptions (Glue { msg, get }) subscriptions_ mainSubscriptions =
    \model ->
        Sub.batch
            [ mainSubscriptions model
            , Sub.map msg <| subscriptions_ <| get model
            ]


{-| Subscribe to subscriptions when model is in some state.

    type alias Model =
        { subModuleSubsOn : Bool
        , subModuleModel : SubModule.Model
        }

    subscriptions : Model -> Sub Msg
    subscriptions =
        (\_ -> Mouse.clicks Clicked)
            |> Glue.subscriptionsWhen .subModuleSubOn subModule SubModule.subscriptions

-}
subscriptionsWhen : (model -> Bool) -> Glue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptionsWhen cond g subscriptions_ mainSubscriptions model =
    if cond model then
        subscriptions g subscriptions_ mainSubscriptions model

    else
        mainSubscriptions model


{-| Render submodule's view.

    view : Model -> Html msg
    view model =
        Html.div []
            [ Html.text model.message
            , Glue.view counter Counter.view model
            ]

-}
view : Glue model subModel msg subMsg -> (subModel -> Html subMsg) -> model -> Html msg
view (Glue rec) v model =
    Html.map rec.msg <| v <| rec.get model



-- Helpers


{-| Tiny abstraction over [`Cmd.map`](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#map)
packed in `(model, Cmd msg)`.
-}
map : (subMsg -> msg) -> ( subModel, Cmd subMsg ) -> ( subModel, Cmd msg )
map constructor pair =
    Tuple.mapSecond (Cmd.map constructor) pair
