module Glue
    exposing
        ( Glue
        , simple
        , poly
        , glue
        , init
        , update
        , view
        , withView
        , subscriptions
        , subscriptionsWhen
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

@docs init, update, view, withView, subscriptions, subscriptionsWhen

# Helpers

@docs map

-}

import Html exposing (Html, text)


{-| `Glue` defines interface mapings between parent and child module.

You can create `Glue` with the [`simple`](#simple), [`poly`](#poly) or [`glue`](#glue) function constructor in case of non-standard APIs.
Every glue layer is defined in terms of `Model`, `[Submodule].Model` `Msg` and `[Submodule].Msg`.
-}
type Glue model subModel msg subMsg
    = Glue
        { model : subModel -> model -> model
        , init : ( subModel, Cmd msg )
        , update : subMsg -> model -> ( subModel, Cmd msg )
        , view : model -> Html msg
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
    , init : ( subModel, Cmd subMsg )
    , update : subMsg -> subModel -> ( subModel, Cmd subMsg )
    , view : subModel -> Html subMsg
    , subscriptions : subModel -> Sub subMsg
    }
    -> Glue model subModel msg subMsg
```
-}
simple :
    { msg : subMsg -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    , init : ( subModel, Cmd subMsg )
    , update : subMsg -> subModel -> ( subModel, Cmd subMsg )
    , view : subModel -> Html subMsg
    , subscriptions : subModel -> Sub subMsg
    }
    -> Glue model subModel msg subMsg
simple { msg, get, set, init, update, view, subscriptions } =
    Glue
        { model = set
        , init = init |> map msg
        , update =
            \subMsg model ->
                get model
                    |> update subMsg
                    |> map msg
        , view =
            \model ->
                get model
                    |> view
                    |> Html.map msg
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
    , init : ( subModel, Cmd msg )
    , update : subMsg -> subModel -> ( subModel, Cmd msg )
    , subscriptions : subModel -> Sub msg
    }
    -> Glue model subModel msg subMsg
```
-}
poly :
    { get : model -> subModel
    , set : subModel -> model -> model
    , init : ( subModel, Cmd msg )
    , update : subMsg -> subModel -> ( subModel, Cmd msg )
    , subscriptions : subModel -> Sub msg
    }
    -> Glue model subModel msg subMsg
poly { get, set, init, update, subscriptions } =
    Glue
        { model = set
        , init = init
        , update =
            \subMsg model ->
                get model
                    |> update subMsg
        , view = \model -> text ""
        , subscriptions =
            \model ->
                get model
                    |> subscriptions
        }


{-| Helper for add custom view when using [`poly`](#poly) or [`simple`](#simple) constructor.



**Interface:**

```
withView :
    (model -> Html msg)
    -> Glue model subModel msg subMsg
    -> Glue model subModel msg subMsg
```
-}
withView : (model -> Html msg) -> Glue model subModel msg subMsg -> Glue model subModel msg subMsg
withView view (Glue data) =
    Glue { data | view = view }


{-| Low level [Glue](#Glue) constructor.

Useful when you can't use either [`simple`](#simple) or [`poly`](#poly).
This can be caused by nonstandard API where one of the functions uses generic `msg` and other `SubModule.Msg`.

*Always use this constructor as your last option for constructing [`Glue`](#Glue).*

**Interface:**

```
glue :
    { model : subModel -> model -> model
    , init : ( subModel, Cmd msg )
    , update : subMsg -> model -> ( subModel, Cmd msg )
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }
    -> Glue model subModel msg subMsg
```
-}
glue :
    { model : subModel -> model -> model
    , init : ( subModel, Cmd msg )
    , update : subMsg -> model -> ( subModel, Cmd msg )
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }
    -> Glue model subModel msg subMsg
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
init : Glue model subModel msg subMsg -> ( subModel -> a, Cmd msg ) -> ( a, Cmd msg )
init (Glue { init }) ( fc, cmd ) =
    let
        ( subModel, subCmd ) =
            init
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
update : Glue model subModel msg subMsg -> subMsg -> ( model, Cmd msg ) -> ( model, Cmd msg )
update (Glue { update, model }) subMsg ( m, cmd ) =
    let
        ( subModel, subCmd ) =
            update subMsg m
    in
        ( model subModel m, Cmd.batch [ subCmd, cmd ] )


{-| Render submodule's view.

```
view : Model -> Html msg
view model =
    Html.div []
        [ Html.text model.message
        , Glue.view counter model
        ]
```
-}
view : Glue model subModel msg subMsg -> model -> Html msg
view (Glue { view }) =
    view


{-| Subscribe to subscriptions defined in submodule.

```
subscriptions : Model -> Sub Msg
subscriptions =
    (\model -> Mouse.clicks Clicked)
        |> Glue.subscriptions subModule
        |> Glue.subscriptions anotherNestedModule
```
-}
subscriptions : Glue model subModel msg subMsg -> (model -> Sub msg) -> (model -> Sub msg)
subscriptions (Glue { subscriptions }) mainSubscriptions =
    \model -> Sub.batch [ mainSubscriptions model, subscriptions model ]


{-| Subscribe to subscriptions when model is in some state.

```
subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Mouse.clicks Clicked)
        |> Glue.subscriptionsWhen .subModuleSubOn subModule
```
-}
subscriptionsWhen : (model -> Bool) -> Glue model subModel msg subMsg -> (model -> Sub msg) -> (model -> Sub msg)
subscriptionsWhen cond glue sub model =
    if cond model then
        subscriptions glue sub model
    else
        sub model



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
        { model = \subModel model -> { model | counter = subModel }
        , init = Counter.init |> Glue.map CounterMsg
        , update =
            \subMsg model ->
                Counter.update subMsg model.counter
                    |> Glue.map CounterMsg
        , view = \model -> Html.map CounterMsg <| Counter.view model.counter
        , subscriptions = \_ -> Sub.none
        }
```
-}
map : (subMsg -> msg) -> ( subModel, Cmd subMsg ) -> ( subModel, Cmd msg )
map constructor ( subModel, subCmd ) =
    ( subModel, Cmd.map constructor subCmd )
