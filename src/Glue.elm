module Glue exposing
    ( Glue
    , simple, poly, glue
    , init
    , update, updateModel, updateWith, updateModelWith
    , subscriptions, subscriptionsWhen
    , view, viewSimple
    , map
    )

{-| Composing Elm applications from smaller isolated parts (modules).
You can think of this as a lightweight abstraction built around `(model, Cmd msg)` and
`(model, Sub msg)` pairs, composing `init`, `update`, `view` and `subscribe` using
[`Cmd.map`](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#map),
[`Sub.map`](https://package.elm-lang.org/packages/elm/core/latest/Platform-Sub#map)
and [`Html.map`](https://package.elm-lang.org/packages/elm/html/latest/Html#map).

It's recommended to avoid usage of pattern with stateful modules unless there is
clear benefit to choose it. In cases where one would like to use `Cmd.map`
pattern anyway though, Glue can be used to avoid repeatable patterns for mapping
the `msg` types and updating models.


# Data type

@docs Glue


# Constructors

@docs simple, poly, glue


# Init

Designed for chaining initialization of child modules from parent `init` function.

@docs init


# Updates

@docs update, updateModel, updateWith, updateModelWith


# Subscriptions

@docs subscriptions, subscriptionsWhen


# View

@docs view, viewSimple


# Helpers

@docs map

-}

import Html exposing (Html)


{-| `Glue` describes an interface between the parent and child module.

You can create `Glue` with the [`simple`](#simple), [`poly`](#poly) or [`glue`](#glue) function constructors.
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


{-| General [`Glue`](#Glue) constructor.
-}
glue :
    { msg : subMsg -> msg
    , get : model -> subModel
    , set : subModel -> model -> model
    }
    -> Glue model subModel msg subMsg
glue rec =
    Glue rec


{-| Simple [`Glue`](#Glue) constructor for modules that don't produce Cmds.

**Note that with this constructor you won't be able to use some functions
provided within this library.**

-}
simple :
    { get : model -> subModel
    , set : subModel -> model -> model
    }
    -> Glue model subModel Never Never
simple rec =
    Glue
        { msg = Basics.never
        , get = rec.get
        , set = rec.set
        }


{-| A specialized [`Glue`](#Glue) constructor. Useful when the module's API has
generic `msg` type and maps Cmds etc. internally.

This constructor will do nothing to the child `msg`s.

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
    ( fc subModel
    , Cmd.batch
        [ cmd
        , Cmd.map msg subCmd
        ]
    )


{-| Call the child module `update` function with a given message. Useful for
nesting update calls. This function expects the child `update` to work with `Cmd`s.

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
                    |> Glue.update counter updateCounter counterMsg

-}
update : Glue model subModel msg subMsg -> (a -> subModel -> ( subModel, Cmd subMsg )) -> a -> ( model, Cmd msg ) -> ( model, Cmd msg )
update (Glue rec) fc msg ( model, cmd ) =
    let
        ( subModel, subCmd ) =
            fc msg <| rec.get model
    in
    ( rec.set subModel model
    , Cmd.batch
        [ Cmd.map rec.msg subCmd
        , cmd
        ]
    )


{-| Call the child module `update` function with a given message. This function
expects the child `update` to _not_ work with `Cmd`s.

Note you can use different functions than the child's main `update`. For example
the child module might have an `updateForRouteChange` function specialized
for a specific parent module situation - you can plug it in here too!

    -- Child module
    updateCounter : Counter.Msg -> Counter.Model -> Counter.Model
    updateCounter msg model =
        case msg of
            Increment ->
                model + 1

    -- Parent module
    update : Msg -> Model -> Model
    update msg model =
        case msg of
            CounterMsg counterMsg ->
                Glue.updateModel counter updateCounter counterMsg model

-}
updateModel : Glue model subModel msg subMsg -> (a -> subModel -> subModel) -> a -> model -> model
updateModel (Glue rec) fc msg model =
    rec.set (fc msg <| rec.get model) model


{-| Updates the child module with a function other than `update`. This function
expects the child function to work with `Cmd`s.

    increment : Counter.Model -> ( Counter.Model, Cmd Counter.Msg )
    increment model =
        ( model + 1, Cmd.none )

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            IncrementCounter ->
                ( model, Cmd.none )
                    |> Glue.updateWith counter increment

-}
updateWith : Glue model subModel msg subMsg -> (subModel -> ( subModel, Cmd subMsg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateWith (Glue rec) fc ( model, cmd ) =
    let
        ( subModel, subCmd ) =
            fc <| rec.get model
    in
    ( rec.set subModel model, Cmd.batch [ Cmd.map rec.msg subCmd, cmd ] )


{-| Updates the child module with a function other than `update`. This function
expects the child function to _not_ work with `Cmd`s.

    increment : Counter.Model -> ( Counter.Model, Cmd Counter.Msg )
    increment model =
        ( model + 1, Cmd.none )

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            IncrementCounter ->
                ( model, Cmd.none )
                    |> Glue.updateWith counter increment

-}
updateModelWith : Glue model subModel msg subMsg -> (subModel -> subModel) -> model -> model
updateModelWith (Glue rec) fc model =
    rec.set (fc <| rec.get model) model


{-| Subscribe to the `subscriptions` defined in the child module.

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


{-| Subscribe to child's `subscriptions` based on some condition in the parent module.

    type alias Model =
        { useCounter : Bool
        , counterModel : Counter.Model
        }

    subscriptions : Model -> Sub Msg
    subscriptions =
        (\_ -> Mouse.clicks Clicked)
            |> Glue.subscriptionsWhen .useCounter counter Counter.subscriptions

-}
subscriptionsWhen : (model -> Bool) -> Glue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptionsWhen cond g subscriptions_ mainSubscriptions model =
    if cond model then
        subscriptions g subscriptions_ mainSubscriptions model

    else
        mainSubscriptions model


{-| Render child module's view.

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


{-| View the `Glue` constructed with the [`simple`](#simple) constructor.

Because the `Msg` is not part of the Glue definition (`Never` type) it needs
to be passed in as a argument.

-}
viewSimple : Glue model subModel Never Never -> (subModel -> Html subMsg) -> (subMsg -> msg) -> model -> Html msg
viewSimple (Glue rec) v msg model =
    Html.map msg <| v <| rec.get model



-- Helpers


{-| A tiny abstraction over [`Cmd.map`](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#map)
packed in `(model, Cmd msg)`.

    -- Parent module
    type Msg
        = ChildMsg Child.Msg

    update =
        -- ...
        childModelCmdPair
            |> Glue.map ChildMsg

-}
map : (subMsg -> msg) -> ( subModel, Cmd subMsg ) -> ( subModel, Cmd msg )
map constructor pair =
    Tuple.mapSecond (Cmd.map constructor) pair
