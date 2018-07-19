module Glue
    exposing
        ( Glue
        , simple
        , poly
        , glue
        , init
        , update
        , view
        , subscriptions
        , subscriptionsWhen
        , updateWith
        , trigger
        , updateWithTrigger
        , map
        )

{-| Composing Elm applications from smaller isolated parts (modules).
You can think about this as about lightweight abstraction built around `(model, Cmd msg)` pair
that reduces boilerplate required for composing `init` `update` `view` and `subscribe` using
[`Cmd.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#map),
[`Sub.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Sub#map)
and [`Html.map`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#map).

# Datatype Definition

@docs Glue

# Constructors

@docs simple, poly, glue

# Basics

@docs init, update, view, subscriptions, subscriptionsWhen

# Custom Operations

@docs updateWith, trigger, updateWithTrigger

# Helpers

@docs map

-}

import Html exposing (Html)


{-| `Glue` defines interface mapings between parent and child module.

You can create `Glue` with the [`simple`](#simple), [`poly`](#poly) or [`glue`](#glue) function constructor in case of non-standard APIs.
Every glue layer is defined in terms of `Model`, `[Submodule].Model` `Msg`, `[Submodule].Msg` and `a`.

- `model` is `Model` of parent
- `subModel` is `Model` of child
- `msg` is `Msg` of parent
- `subMsg` is `Msg` of child
- `a` is type of `Msg` child's views return in `Html a`. Usually it's either `msg` or `subMsg`.
-}
type Glue model subModel msg subMsg a
    = Glue
        { msg : a -> msg
        , get : model -> subModel
        , set : subModel -> model -> model
        , init : () -> ( subModel, Cmd msg )
        , update : subMsg -> model -> ( subModel, Cmd msg )
        , subscriptions : model -> Sub msg
        }


{-| Simple [`Glue`](#Glue) constructor.

Generally useful for composing independent TEA modules together.
If your module's API is polymofphic use [`poly`](#poly) constructor instead.

**Interface:**

```
simple :
    { msg : subMsg -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    , init : () -> ( subModel, Cmd subMsg )
    , update : subMsg -> subModel -> ( subModel, Cmd subMsg )
    , subscriptions : subModel -> Sub subMsg
    }
    -> Glue model subModel msg subMsg subMsg
```
-}
simple :
    { msg : subMsg -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    , init : () -> ( subModel, Cmd subMsg )
    , update : subMsg -> subModel -> ( subModel, Cmd subMsg )
    , subscriptions : subModel -> Sub subMsg
    }
    -> Glue model subModel msg subMsg subMsg
simple { msg, get, set, init, update, subscriptions } =
    Glue
        { msg = msg
        , get = get
        , set = set
        , init = map msg << init
        , update =
            \subMsg model ->
                get model
                    |> update subMsg
                    |> map msg
        , subscriptions =
            \model ->
                get model
                    |> subscriptions
                    |> Sub.map msg
        }


{-| Polymorphic [`Glue`](#Glue) constructor.

Usefull when module's api has generic `msg` type. Module can also perfrom action bubbling to parent.

**Interface:**

```
poly :
    { get : model -> subModel
    , set : subModel -> model -> model
    , init : () -> ( subModel, Cmd msg )
    , update : subMsg -> subModel -> ( subModel, Cmd msg )
    , subscriptions : subModel -> Sub msg
    }
    -> Glue model subModel msg subMsg msg
```
-}
poly :
    { get : model -> subModel
    , set : subModel -> model -> model
    , init : () -> ( subModel, Cmd msg )
    , update : subMsg -> subModel -> ( subModel, Cmd msg )
    , subscriptions : subModel -> Sub msg
    }
    -> Glue model subModel msg subMsg msg
poly { get, set, init, update, subscriptions } =
    Glue
        { msg = identity
        , get = get
        , set = set
        , init = init
        , update =
            \subMsg model ->
                get model
                    |> update subMsg
        , subscriptions =
            \model ->
                get model
                    |> subscriptions
        }


{-| Low level [Glue](#Glue) constructor.

Useful when you can't use either [`simple`](#simple) or [`poly`](#poly).
This can be caused by nonstandard API where one of the functions uses generic `msg` and other `SubModule.Msg`.

*Always use this constructor as your last option for constructing [`Glue`](#Glue).*

**Interface:**

```
glue :
    { msg : a -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    , init : () -> ( subModel, Cmd msg )
    , update : subMsg -> model -> ( subModel, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Glue model subModel msg subMsg a
```
-}
glue :
    { msg : a -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    , init : () -> ( subModel, Cmd msg )
    , update : subMsg -> model -> ( subModel, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Glue model subModel msg subMsg a
glue =
    Glue



-- Basics


{-| Initialize child module in parent.

```
type alias Model =
    { message : String
    , firstCounterModel : Counter.Model
    , secondCounterModel : Counter.Model
    }

init : ( Model, Cmd msg )
init =
    ( Model "", Cmd.none )
        |> Glue.init firstCounter
        |> Glue.init secondCounter
```
-}
init : Glue model subModel msg subMsg a -> ( subModel -> b, Cmd msg ) -> ( b, Cmd msg )
init (Glue { init }) ( fc, cmd ) =
    let
        ( subModel, subCmd ) =
            init ()
    in
        ( fc subModel, Cmd.batch [ cmd, subCmd ] )


{-| Update submodule's state using it's `update` function.

```
type Msg
    = CounterMsg Counter.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( { model | message = "Counter has changed" }, Cmd.none )
                |> Glue.update counter counterMsg
```

-}
update : Glue model subModel msg subMsg a -> subMsg -> ( model, Cmd msg ) -> ( model, Cmd msg )
update (Glue { update, set }) subMsg ( m, cmd ) =
    let
        ( subModel, subCmd ) =
            update subMsg m
    in
        ( set subModel m, Cmd.batch [ subCmd, cmd ] )


{-| Render submodule's view.

```
view : Model -> Html msg
view model =
    Html.div []
        [ Html.text model.message
        , Glue.view counter Counter.view model
        ]
```
-}
view : Glue model subModel msg subMsg a -> (subModel -> Html a) -> model -> Html msg
view (Glue { msg, get }) view =
    Html.map msg << view << get


{-| Subscribe to subscriptions defined in submodule.

```
subscriptions : Model -> Sub Msg
subscriptions =
    (\model -> Mouse.clicks Clicked)
        |> Glue.subscriptions subModule
        |> Glue.subscriptions anotherNestedModule
```
-}
subscriptions : Glue model subModel msg subMsg a -> (model -> Sub msg) -> (model -> Sub msg)
subscriptions (Glue { subscriptions }) mainSubscriptions =
    \model -> Sub.batch [ mainSubscriptions model, subscriptions model ]


{-| Subscribe to subscriptions when model is in some state.

```
type alias Model =
     { subModuleSubsOn : Bool
     , subModuleModel : SubModule.Model }

subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Mouse.clicks Clicked)
        |> Glue.subscriptionsWhen .subModuleSubOn subModule
```
-}
subscriptionsWhen : (model -> Bool) -> Glue model subModel msg subMsg a -> (model -> Sub msg) -> (model -> Sub msg)
subscriptionsWhen cond glue sub model =
    if cond model then
        subscriptions glue sub model
    else
        sub model



-- Custom Operations


{-| Use child's exposed function to update it's model

```
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixl 0 =>

incrementBy : Int -> Counter.Model -> Counter.Model
incrementBy num model =
    model + num

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementBy10 ->
          model
              |> Glue.updateWith counter (incrementBy 10)
              => Cmd.none
```
-}
updateWith : Glue model subModel msg subMsg a -> (subModel -> subModel) -> model -> model
updateWith (Glue { get, set }) fc model =
    let
        subModel =
            fc <| get model
    in
        set subModel model


{-| Trigger Cmd in by child's function

*Commands are async. Therefor trigger don't make any update directly.
Use [`updateWith`](#updateWith) over `trigger` when you can.*

```
triggerIncrement : Counter.Model -> Cmd Counter.Msg
triggerIncrement _ ->
    Task.perform identity <| Task.succeed Counter.Increment

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementCounter ->
            ( model, Cmd.none )
                |> Glue.trigger counter triggerIncrement
```
-}
trigger : Glue model subModel msg subMsg a -> (subModel -> Cmd a) -> ( model, Cmd msg ) -> ( model, Cmd msg )
trigger (Glue { msg, get }) fc ( model, cmd ) =
    ( model, Cmd.batch [ Cmd.map msg <| fc <| get model, cmd ] )


{-| Similar to [`update`](#update) but using custom function.

```
increment : Counter.Model -> ( Counter.Model, Cmd Counter.Msg )
increment model =
   ( model + 1, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
          IncrementCounter ->
            ( model, Cmd.none )
                |> Glue.updateWithTrigger counter increment
```
-}
updateWithTrigger : Glue model subModel msg subMsg a -> (subModel -> ( subModel, Cmd a )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateWithTrigger (Glue { msg, get, set }) fc ( model, cmd ) =
    let
        ( subModel, subCmd ) =
            fc <| get model
    in
        ( set subModel model, Cmd.batch [ Cmd.map msg subCmd, cmd ] )



-- Helpers


{-| Tiny abstraction over [`Cmd.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#map)
packed in `(model, Cmd msg)` pair that helps you to reduce boilerplate while turning generic TEA app to [`Glue`](#Glue) using [`glue`](#glue) constructor.

This function is generally usefull for turning update and init functions in [`Glue`](#glue) definition.

```
type alias Model =
    { message : String
    , counter : Counter.Model
    }

type Msg
    = CounterMsg Counter.Msg

-- this works liske `simple` constructor
counter : Glue Model Counter.Model Msg Counter.Msg
counter =
    Glue.glue
        { msg = CounterMsg
        , get = .counterModel
        , set = \subModel model -> { model | counterModel = subModel }
        , init = \_ -> Counter.init |> Glue.map CounterMsg
        , update =
            \subMsg model ->
                Counter.update subMsg model.counterModel
                    |> Glue.map CounterMsg
        , subscriptions = \_ -> Sub.none
        }
```
-}
map : (subMsg -> msg) -> ( subModel, Cmd subMsg ) -> ( subModel, Cmd msg )
map constructor ( subModel, subCmd ) =
    ( subModel, Cmd.map constructor subCmd )
