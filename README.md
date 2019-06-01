# Glue

[![Build Status](https://travis-ci.org/turboMaCk/glue.svg?branch=master)](https://travis-ci.org/turboMaCk/glue)

This package helps you reduce boilerplate when composing TEA-based (The Elm Architecture) applications using
`Cmd.map`, `Sub.map` and `Html.map`.
`Glue` is just a thin abstraction over these functions so it's easy to plug it in and out.

**This package is highly experimental and might change a lot over time.**

Feedback and contributions to both code and documentation are very welcome.

[See demo](https://turbomack.github.io/glue/)

## Important Note!

This package is not necessarily designed for either code splitting or reuse but rather for **state separation**.
State separation might or might not be important for reusing certain parts of application.
Not everything is necessarily stateful. For instance many UI parts can be expressed just by using `view` function
to which you pass `msg` constructors (`view : msg -> Model -> Html msg` for instance) and let consumer manage its state.
On the other hand some things like larger parts of applications or parts containing a lot of self-maintainable stateful logic
can benefit from state isolation since it reduces state handling imposed on consumer of that module.
Generally it's a good rule of thumb to always choose simpler approach (And using stateless abstraction is usually simpler) -
*if you aren't sure if you can benefit from extra isolation don't use it.* Always try to define as much logic as you can
using just simple functions and data. Then you can think about possible state separation in places where too much of it is exposed.
**First rule is to avoid breaking of [single source of truth principle](https://en.wikipedia.org/wiki/Single_source_of_truth)**.
If you find yourself synchronizing some state from one place to another than that state shouldn't be probably isolated in first place.


## tl;dr

This package is a result of my experience with building larger single page application in Elm where some modules live in isolation from others.
The goals and features of this package are:

- Reduce boilerplate in `update` and `init` functions.
- Reduce code flow indirection in gluing between parent and child module.
- Define gluing logic in consumer module.
- Enforce common interface in `init`, `update`, `subscribe` and `view`.
- Make updates of nested models composable
- *You should read the whole README anyway.*

## Install

As you would expect...

```
$ elm install turboMaCk/glue
```

## Examples

The best place to start is probably to have a look at [examples](https://github.com/turboMaCk/glue/tree/master/examples).

In particular, you can find examples of:

- [Composing Isolated Elm Apps together using Glue](https://github.com/turboMaCk/glue/tree/master/examples/Counter)
- [Composing Modules with Subscriptions](https://github.com/turboMaCk/glue/tree/master/examples/Subscriptions)
- [Action Bubbling (Sending Actions from Child to Parent)](https://github.com/turboMaCk/glue/tree/master/examples/Bubbling)

## Why?

TEA is an awesome way to write UI apps in Elm. However, not every application
should be defined just in terms of single `Model` and `Msg`.
Basic separation of [`Browser.element`](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element) is really nice
but in some cases these functions as well as `Model` and `Msg` types tend to
grow pretty quickly in an unmanageable way so you need to start breaking things
down.

There are [many ways](https://www.reddit.com/r/elm/comments/5jd2xn/how_to_structure_elm_with_multiple_models/dbkpgbd/)
you can go about it. In particular the rest of this document will focus just on
[separation of concerns](https://en.wikipedia.org/wiki/Separation_of_concerns).
This technique is useful for isolating parts that really don't need to know too
much about each other.
It reduces scope of things a particular module can operate with so the number of
things the programmer has to reason about while adding or changing behaviour is
lower. In TEA this is especially touching the `Msg` and `Model` types and `update`
functions for managing the state.

**It's important to understand that `init`, `update`, `view` and `subscriptions`
are all isolated functions connected via `Browser.element`.
In pure functional programming we're "never" really managing state ourselves but
are rather composing functions that takes state as data and produce new version
of it (`update` function in TEA).**

Now let's have a look at how we can use `Cmd.map`, `Sub.map` and `Html.map` for
concern separation in Elm application. We will nest `init`, `update`,
`subscriptions` and `view` one into another and `map` them from child's to
parent's types. Parent module is then using these units to manage just a subset
of its overall state (`Model`). Here is how `Model` and `Msg` types of a parent
application might look like:

```elm
import SubModule

type alias Model =
    { ...
    , subModuleModel : SubModule.Model
    , ...
    }

type Msg
    = ...
    | SubModuleMsg SubModule.Msg
    | ...
```

Basically, the parent module only holds the `Model` of a child module
(`SubModule`) as a single value, and wraps its `Msg` inside one of its own `Msg`
constructors (`SubModuleMsg`). Of course, `init`, `update` and `subscriptions`
also have to know how to work with this part of `Model`, and there you need
`Cmd.map`, `Sub.map` and `Html.map`. For instance, this is how simple delegation
of `Msg` in `update` might look:

```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ...
        SubModuleMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    SubModule.update subMsg model.subModuleModel
            in
                ( { model | subModuleModel = subModel }, Cmd.map SubModuleMsg subCmd )
        ...
```

As you can see, this is quite neat even though it requires some boiler-plate
code to deconstruct the pair and construct new one. One can as well utilize
`Tuple.mapFirst` and `Tuple.mapSecond` function (bifunctor interface):

```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ...
        SubModuleMsg subMsg ->
            SubModule.update subMsg model.subModuleModel
                |> Tuple.mapFirst (\subModel -> { model | subModel = subModel })
                |> Tuple.mapSecond (Cmd.map SubModuleMsg)
        ...
```
Let's take a look at `view` and [`Html.map`](https://package.elm-lang.org/packages/elm/html/latest/Html#map) now:

```elm
view : Model -> Html Msg
view model =
   Html.div []
       [ ...
       , Html.map SubModuleMsg <| SubModule.view model.subModuleModel
       , ...
       ]
```

You can use `Cmd.map` inside `init` as well as `Sub.map` which is fairly similar
to `Html.map` in `subscriptions` to finish wiring of a child module (`SubModule`).

And this is as far as pure TEA goes. This may possibly be good fit for your needs,
and that's OK. Why might you still want to use this package?

- It gives you the ability to define mapping updates of modules and Cmd/Sub mapping in single place (DRY).
- It simplifies the routine code in functions delegating to child modules
- Enforces common type interface for functions expressed from modules

## How?

**The most important type that TEA is built around is `( Model, Cmd Msg )`.
All we're missing is just a tiny abstraction that will make working with this
pair easier. This is really the core idea of the whole `Glue` package.**

To simplify gluing of things together, this package introduces the `Glue` type.
This is simply just a name-space for pure functions that defines interface
between modules to which you can then refer by single name. Other functions
within the `Glue` package use the `Glue.Glue` type as proxy to access these functions.

> Note that Glue has essentially 2 parts. The first one is simple Lens for model updates.
> The second is writer of effects (Cmds, Subscriptions).

### Gluing independent TEA App

This is how we can construct the `Glue` type for [counter example](https://guide.elm-lang.org/architecture/buttons.html):

```elm
import Glue exposing (Glue)
import Counter

counter : Glue Model Counter.Model Msg Counter.Msg
counter =
    Glue.glue
        { msg = CounterMsg
        , get = .counterModel
        , set = \subModel model -> { model | counterModel = subModel }
        }
```
All mappings from one type to another (`Model` and `Msg` of parent/child) will
happen as defined in this type. Definition of this interface depends on API of
child module (`Counter` in this case).

With `Glue` defined, we can go and integrate it to rest of the logic.
Based on the `Glue` type definition, we know we're expecting `Model` and `Msg`
to be (at least) as following:

```elm
type alias Model =
    { counterModel : Counter.Model }

type Msg
    = CounterMsg Counter.Msg
```

Now we can define init, update and view functions:

```elm

init : ( Model, Cmd Msg )
init =
    ( Model, Cmd.none )
        |> Glue.init counter Counter.init

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( model, Cmd.none )
                |> Glue.update counter Counter.update counterMsg

view : Model -> Html Msg
view =
    Glue.view counter Counter.view
```

As you can see we're using just `Glue.init`, `Glue.update` and `Glue.view`
in these functions to wire child module. Also compared to the original TEA example
we can easily update parent Model an generate additional parent Cmd as well.

> This version of counter is using `Browser.element` type of interface as opposed
> to `Browser.sandbox`. It is possible to use `Glue` with `sandbox` type of interface.
> See the [counter example](https://github.com/turboMaCk/glue/tree/master/examples/Counter) and the `Glue.simple` constructor.

### Wrap Polymorphic Module

We're going to be using term "polymorphic" just because of the lack of better
name. What we really mean is a module which already translates its inner `Msg`
to some `a` by function provided from parent module.

Such module can also be generating parent messages using function passed into
its functions making it possible to (asynchronously) notify parent module about
certain events.

To make the `Counter` example "polymorphic" we start by adding one extra argument
to its `view` function and use `Html.map` internally. Then we need to change type
annotation of `init` and `update` to `Cmd msg`. Since both function are using
just `Cmd.none` we don't need to change anything else but that.

```elm
init : ( Model, Cmd msg )

update : msg -> Model -> ( Model, Cmd msg )

view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
    Html.map toMsg <|
        Html.div []
            [ Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
            , Html.text <| toString model
            , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
            ]
```

> As you can see `view` is now taking an extra argument - function from `Msg` to parent's `msg`.
> In practice it's usually good to use record with functions called `Config msg` which will be much more extensible.

Now we need to change `Glue` type definition in parent module to reflect the new API of `Counter`:

```elm
counter : Glue Model Counter.Model Msg Msg
counter =
    Glue.poly
        { get = .counterModel
        , set = \subModel model -> { model | counterModel = subModel }
        }
```

As you can see, we've switched from `Glue.glue` function to `Glue.poly`.
Also the type signature now contains `Msg` twice as the type produced by the
child module is the `Msg` of parent module. In fact `Glue.poly` is just
a constructor that defines `msg` as `identity`.

We also need to change parent's view since its API has changed and we need to
pass an extra argument now:

```elm
view : Model -> Html Msg
view =
    Glue.view counter (Counter.view CounterMsg)
```

### Child-parent Communication

If your module is [polymorphic](#wrap-polymorphic-module) it can easily send `Cmd`s to its parent.
Please check [cmd-extra](http://package.elm-lang.org/packages/GlobalWebIndex/cmd-extra/latest) package
which helps you construct `Cmd Msg` from `Msg`.

**It's important to understand that this might not be the best technique for
managing all communication between parent and child. You can always expose `Msg`
constructor from child (`exposing (Msg(..))`) and pattern-match on it in parent.
If you need to do such a thing you might have made a design mistake (improper
separation of state). Do these states really need to be separated? In most cases
communicating with parent in async fashion makes it easier to reason about data
flow in the app but there is no silver bullet. You know the best what best
applies for your case.**

Using `Cmd` for communication with parent module works like this:

```text
    +------------------------------------+
    |                                    |
    v                                    |
+-----------------------------------+    |
|                                   |    |
| Parent Module                     |    |
|                                   |    +
|   +                               |  Cmd Msg
|   |                               |    |
|  Model                            |    |
|   |                               |    |
|   |   +------------------------+  |    |
|   |   |                        |  |    |
|   |   | Child Module           |  |    |
|   |   |                        |  |    |
|   +-> |                        +-------+
|       +------------------------+  |
|                                   |
+-----------------------------------+

```

As an example, we can use the (polymorphic) `Counter.elm` again.
Let's say we want to send some action to the parent whenever its model (count) changes.

For this we need to define a helper function in `Counter.elm`:

```elm
-- this uses `GlobalWebIndex/cmd-extra`
import Cmd.Extra

notify : (Int -> msg) -> Int -> Cmd msg
notify toMsg count =
    Cmd.Extra.perform <| toMsg count
```

`notify` takes the parent's `Msg` constructor that is expecting integer as an argument and performs it as `Cmd`.

Now we need to change `init` and `update` so they're emitting this new `Cmd`.
The simplest way is just to make them both accept a `msg` constructor.

```elm
init : (Int -> msg) -> ( Model, Cmd msg )
init toMsg =
    let
        model =
            0
    in
    ( 0, notify toMsg model )


update : (Int -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toParentMsg msg model =
    let
        newModel =
            case msg of
                Increment ->
                    model + 1

                Decrement ->
                    model - 1
    in
        ( newModel, notify toParentMsg newModel )
```

Now both `init` and `update` should send `Cmd` after `Model` is updated.
This is a breaking change to `Counter`'s API (an extra argument) so we need to change its integration as well.
But since we want to actually use this message and do something with it let's first update
the parent's `Model` and `Msg`:

```elm
type alias Model =
    { max : Int
    , counter : Counter.Model
    }

type Msg
    = CounterMsg Counter.Msg
    | CountChanged Int
```

Because we've changed `Model` (added `max : Int`) we should change `init`
and probably render max value in `view` of parent as well:

```elm
init : ( Model, Cmd Msg )
init =
    ( Model 0, Cmd.none )
        |> Glue.init counter Counter.init

view : Model -> Html Msg
view model =
    Html.div []
        [ Glue.view counter (Counter.view CounterMsg) model
        , Html.text <| "Max historic value: " ++ toString model.max
        ]
```

This completes the changes to `Model`. Now we need to change update `update` function
so it can handle the `CountChanged` message.

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( model , Cmd.none )
                |> Glue.update counter (Counter.update CountChanged) counterMsg

        CountChanged num ->
            if num > model.max then
                ( { model | max = num }, Cmd.none )
            else
                ( model, Cmd.none )
```

As you can see we're setting `max` to received int if it's greater than the current value.

See this [complete example](https://github.com/turboMaCk/glue/tree/master/examples/Bubbling) to learn more.

## License

BSD-3-Clause

Copyright 2017-2019 Marek Fajkus
