# Glue

[![Build Status](https://travis-ci.org/turboMaCk/glue.svg?branch=master)](https://travis-ci.org/turboMaCk/glue)

This package helps you to reduce boilerplate while composing TEA-based (The Elm Architecture) applications using
[`Cmd.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#map),
[`Sub.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Sub#map)
and [`Html.map`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#map).
`Glue` is just thin abstraction over these functions so it's easy to plug it in and out.
It's fair to say that `Glue` is an alternative to [elm-parts](http://package.elm-lang.org/packages/debois/elm-parts/latest),
but uses a different approach (no better or worse) for composing isolated pieces/modules together.

**This package is highly experimental and might change a lot over time.**

Feedback and contributions to both code and documentation are very welcome.

## Important Note!

This package is not necessary designed for either code splitting or reuse but rather for **state separation**.
State separation might and might not be important for reusing certain parts of application.
Not everything is necessary stateful. For instance many UI parts can be express just by using `view` function
to which you pass `msg` constructors (`view : msg -> Model -> Html msg` for instance) and let consumer to manage it's state.
On the other hand some things like larger parts of applications or parts containing a lot of self-maintainable stateful logic
can benefit from state isolation since it reduces state handling imposed on consumer of that module.
Generally it's good rule to always choose simpler approach (And using stateless abstraction is usually simpler) -
*If you aren't sure if you can benefit from extra isolation don't use it.* Always try to define as much logic as you can
using just simple functions and data. Then you can think about possible state separation in places where too much of it is exposed.
**First rule is to avoid breaking of [single source of truth principle](https://en.wikipedia.org/wiki/Single_source_of_truth)**.
If you find yourself synchronizing some state from one place to another than that state shouldn't be probably isolated in first place.


## tl;dr

This package is a result of my experience with building larger application in Elm where some modules lives in isolation from others.
The goals and features of this package are:

- Reduce boilerplate in `update` and `init` functions.
- Reduce [indirection](https://en.wikipedia.org/wiki/Indirection) in glueing between parent and child module.
- Define glueing logic on consumer level.
- Enforce common interface in `init` `update` `subscribe` and `view`.

## Install

Is as you would expect...

```
$ elm-package install turboMaCk/glue
```

## Examples

The best place to start is probably to have a look at [examples](https://github.com/turboMaCk/glue/tree/master/examples).

In particular, you can find:

### [Transforming Isolated Elm Apps together using Glue](https://github.com/turboMaCk/glue/tree/master/examples/counter)

### [Composing Modules with Subscriptions](https://github.com/turboMaCk/glue/tree/master/examples/subscriptions)

### [Action Bubbling (Sending Actions from Child to Parent)](https://github.com/turboMaCk/glue/tree/master/examples/bubbling)

## Why?

TEA is an awesome way to write Html-based apps in Elm. However, not every application can be defined just in terms of single `Model` and `Msg`.
Basic separation of [`Html.program`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program) is really nice
but in some cases these functions grow pretty quickly in an unmanageable way so you need to start breaking things.

There are [many ways](https://www.reddit.com/r/elm/comments/5jd2xn/how_to_structure_elm_with_multiple_models/dbkpgbd/)
you can start. In particular rest of this document will focus just on [separation of concerns](https://en.wikipedia.org/wiki/Separation_of_concerns).
This technique is useful for isolating parts that really don't need know too much about each other. It helps to reduce number of things particular module is touching,
has to and can manage while adding or changing behaviour of such a isolated part of system. In tea this is especially touching `Msg` type and `update` function.
Using techniques described below you can split `update` logic and `Msg` type so some modules are partially or fully responsible for updating their own part of overall `Model`.

**It's important to understand that `init` `update` `view` and `subscriptions` are all isolated functions connected via `Html.program`.
In pure functional programming we're "never" really managing state yourself but rather composing functions that takes state as data to produce new version of it.**

Lets have a look on how we can use [`Cmd.map`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#map),
[`Sub.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Sub#map)
and [`Html.map`](http://package.elm-lang.org/packages/debois/elm-parts/latest) for separation in Elm app.
You can use them for nesting `init`, `update`, `subscriptions` and `view`.
Higher level module is then using these units to manage just this subset of his overall state (`Model`).
Here is how the `Model` and `Msg` types of a parent application might look:

```elm
import SubModule

type alias Model =
    { ...
    , subModule: SubModule.Model
    , ...
    }

type Msg
    = ...
    | SubModule SubModule.Msg
    | ...
```

Basically, the top-level module only holds the `Model` of a sub-module as a single value, and wraps its `Msg` inside one of its `Msg` constructors.
Of course, `init`, `update`, and `subscriptions` also have to know how to work with this part of `Model`, and there you need
[`Cmd.map`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#map),
[`Html.map`](http://package.elm-lang.org/packages/debois/elm-parts/latest) and
[`Sub.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Sub#map).
For instance, this is how simple delegation of `Msg` in `update` might look:

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

As you can see, this is quite neat even though it requires some boiler-plate code.
Let's take a look at `view` and [`Html.map`](http://package.elm-lang.org/packages/debois/elm-parts/latest) in action:

```elm
view : Model -> Html Msg
view model =
   Html.div
       []
       [ ...
       , Html.map SubModuleMsg <| SubModule.view model.subModuleModel
       , ... ]
```

You can use [`Cmd.map`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#map) inside `init` as well and
[`Sub.map`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Sub#map) which is fairly similar to
[`Html.map`](http://package.elm-lang.org/packages/debois/elm-parts/latest) in `subscriptions` to finish wiring of a submodule.

And this is as far as pure TEA goes. This may possibly be good fit for your needs, and that's OK. Why might you still want to use this package?

- It helps keep your `update`, `init`, `view` `subscriptions` clean from wiring logic.
- It enforces very abstract interface of mapping between these functions with just little implementation overhead.
- It uses record to keep wiring in single namespace which reduces indirection in interface definition.

## How?

**The most important type that TEA is built around is `( Model, Cmd Msg )`. All we're missing is just a tiny abstraction that will
make working with this pair easier. This is really the core idea of the whole `Glue` package.**

To simplify glueing things together, the `Gue` type is introduced by this package.
This is simply just a namespace for pure functionsthat defines interface between modules to which you can then refer using single name.
Other functions within the `Glue` package then use the `Glue.Glue` type as proxy to access these functions.

### Glueing independent TEA App

This is how we can construct the `Glue` type for [counter example](https://guide.elm-lang.org/architecture/user_input/buttons.html):

```elm
import Glue

counter : Glue Model Counter.Model Msg Counter.Msg
counter =
    Glue.glue
        { model = \subModel model -> { model | counterModel = subModel }
        , init = Counter.init |> Glue.map CounterMsg
        , update =
            \subMsg model ->
                Counter.update subMsg model.counterModel
                    |> Glue.map CounterMsg
        , view = \model -> Html.map CounterMsg <| Counter.view model.counterModel
        , subscriptions = \_ -> Sub.none
        }
```
All mappings from one type to another happens in here. This is different in the case of [polymorphic module](#wrap-polymorphic-module),
but more about this later. With `Glue` defined, we can go and integrate it with the parent.

Based on the `Glue` type definition, we know we're expecting `Model` and `Msg` to be as follows:

```elm
type alias Model =
    { counterModel : Counter.Model }

type Msg
    = CounterMsg Counter.Msg
```

And this is our `init`, `update` and `view`:

```elm

init : ( Model, Cmd msg )
init =
    ( Model, Cmd.none )
        |> Glue.init counter

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( model, Cmd.none )
                |> Glue.update counter counterMsg

view : Model -> Html Msg
view =
    Glue.view counter
```

### Wrap Polymorphic Module

A "polymorphic module" is what I call TEA modules that have to be integrated into some other app *(I know this is not really the best name, ideas?)*.
This basically means they are using `Cmd.map`, `Html.map`, and `Sub.map` internally. Let's make `Counter.elm` polymorphic so it's clear what this mean.
This will require us to add one extra argument to counter's `view` function and a small change to the type annotations of `init` and `update`:

```elm
init : ( Model, Cmd msg )

update : Msg -> Model -> ( Model, Cmd msg )

view : (Msg -> msg) -> Model -> Html msg
view msg model =
    Html.map msg <|
        Html.div
            []
            [ Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
            , Html.text <| toString model
            , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
            ]
```

Then we need to change our `Glue` definition in the parent module to reflect the new API of counter:

```elm
counter : Glue Model Counter.Model Msg Counter.Msg
counter =
    Glue.glue
        { model = \subModel model -> { model | counter = subModel }
        , init = Counter.init
        , update = \subMsg model -> Counter.update subMsg model.counter
        , view = \model -> Counter.view CounterMsg model.counter
        , subscriptions = \_ -> Sub.none
        }
```

This is all that is required when a child module's API changes.
Since the `Glue` type holds all mappings in one place, there is no need for changes in the parent's `init`, `update`, or `view` functions.

### Action Bubbling

If your module is [polymorphic](#Wrap-Polymorphic-Module) you can easily send `Cmd` to its parent.
Please check [cmd-extra](http://package.elm-lang.org/packages/GlobalWebIndex/cmd-extra/latest) package
which helps you construct `Cmd Msg` from `Msg`.

**It's important to understand that this might not be the best technique for managing all communication between parent and child.
You can always expose `Msg` constructor from child (`exposing(Msg(..))`) and match it in parent. Anyway if you need to do such a thing
you maybe made a mistake while designing separation of state. Do these states really need to be separated?**

Using `Cmd` for communication with upper module works like this:

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

As an example, we can use the (polymorphic) `Counter.elm` again. Let's say we want to send some action to the parent when the counter's value is even.
For this we need to define a helper function in `Counter.elm`:

```elm
isEven : Int -> Bool
isEven =
    (==) 0 << (Basics.flip (%)) 2


notifyEven : msg -> Model -> Cmd msg
notifyEven msg model =
    if isEven model then
        Cmd.Extra.perform msg
    else
        Cmd.none
```

`isEven` is pretty straightforward. It just returns `True` or `False` for a given `Int`.
`notifyEven` takes the parent's `Msg` constructor and either [`perform`](http://package.elm-lang.org/packages/GlobalWebIndex/cmd-extra/1.0.0/Cmd-Extra#perform)s
it as `Cmd`, or returns `Cmd.none`.

Now we need to change `init` and `update` so they're emitting this new `Cmd`.
The simplest way is just to make them both accept a `msg` constructor as following:

```elm
init : msg -> ( Model, Cmd msg )
init msg =
    let
        model = 0
    in
        ( model, notifyEven msg model )


update : msg -> Msg -> Model -> ( Model, Cmd msg )
update notify msg model =
    let
        newModel =
            case msg of
                Increment ->
                    model + 1

                Decrement ->
                    model - 1
    in
        ( newModel, notifyEven notify newModel )
```

Now both `init` and `update` should send `Cmd` when `Model` is an even number.
This is a breaking change to `Counter`'s API so we need to change its integration as well.
Since we want to actually use this message and do something with it let me first update the parent's `Msg` and `Model`:

```elm
type alias Model =
    { even : Bool
    , counter : Counter.Model
    }

type Msg
    = CounterMsg Counter.Msg
    | Even
```

Because we've changed `Model` (added `even : Bool`) we should change `init` and `view` like:

```elm
init : ( Model, Cmd Msg )
init =
    ( Model False, Cmd.none )
        |> Glue.init counter

view : Model -> Html Msg
view model =
    Html.div []
        [ Glue.view counter model
        , Html.text
            (if model.even then
                "is even"
            else
                "is odd")
        ]
```

This completes the changes to `Model`. Now we want to update `update` function so it can handle the `Even` message.

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            ( { model | even = False }, Cmd.none )
                |> Glue.update counter counterMsg

        Even ->
            ( { model | even = True }, Cmd.none )
```

As you can see we're setting `even` to `False` on every `CounterMsg`.
This is because `Counter` is just emitting `Cmd` when its `Model` is Even.

To handle the `Even` action itself. This simply sets `even = True` in the model.

*This is to show you why you might need to update both the parent's and child's `Model` on a single `Msg` (`CounterMsg` in this case), and how to do it.
Anyway this is just really simple example. In real world you probably don't want to use `Cmd` for things as like this.*

Since the parent is ready to handle actions from `Counter` our last step is simply to update the `Glue` construction for the new APIs:

```elm
counter : Glue Model Counter.Model Msg Counter.Msg
counter =
    Glue.glue
        { model = \subModel model -> { model | counter = subModel }
        , init = Counter.init Even
        , update = \subMsg model -> Counter.update Even subMsg model.counter
        , view = \model -> Counter.view CounterMsg model.counter
        , subscriptions = \_ -> Sub.none
        }
```

There we simply pass the parent's `Even` constructor to the `update` and `init` functions of the child.
This is all we need to do to wire `Cmd` from child to parent.

See this [complete example](https://github.com/turboMaCk/glue/tree/master/examples/bubbling) to learn more.

## Re-evaluating and Future Work

This package is still in a really early stage of development and needs to be tested in the field.
Personally I still need to sort out a few things.
For instance is it really a good idea to include `view` handling. Or if its API is really right.
First of all `view` is not really related to `(Model, Cmd msg)` pair itself.
Also view is API are usually thing that varies most between modules. On the other hand it's nice
to refer to modules view same way you refer to it's update.
Anyway since this package is still in early experimental stage I'll leave this question open.

## License

BSD-3-Clause

Copyright 2017 Marek Fajkus
