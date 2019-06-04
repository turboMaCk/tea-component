module Glue.Lazy exposing
    ( LazyGlue
    , initLater, forceInit, forceInitModel, ensure, ensureModel
    , update, updateModel, updateWith, updateModelWith, trigger
    , subscriptions, subscriptionsWhen
    , view
    )

{-| In some cases, it makes sense to postpone initialization
of some module.

Such part of state can expressed using `Maybe` type like

    type alias Model
        { about : About.Model
        , userSettings : Maybe UserSettings.Model
        }

or for instance using sum type like

    type Model
        = About About.Model
        | UserSettings UserSettings.Model

In such cases though, the programmer is responsible for manually initializing
the module at the right time.

@docs LazyGlue


## Initialization

@docs initLater, forceInit, forceInitModel, ensure, ensureModel


## Updates

@docs update, updateModel, updateWith, updateModelWith, trigger


## Subscriptions

@docs subscriptions, subscriptionsWhen


## View

@docs view

-}

import Glue exposing (Glue)
import Glue.Internal
import Html exposing (Html)


{-| -}
type alias LazyGlue model subModel msg subMsg =
    Glue.Internal.Glue model (Maybe subModel) msg subMsg


{-| Compatible with Glue.init style initialization but assigns `Nothing` to the part of model that
should be initalized later. This function is useful in cases using record with maybes.
-}
initLater : Glue model subModel msg subMsg -> ( Maybe subModel -> a, Cmd msg ) -> ( a, Cmd msg )
initLater _ ( f, cmd ) =
    ( f Nothing, cmd )


{-| Force intialization of module. If model state already exists in the form of `Just model`
this value is overwrite nontherless
-}
forceInit : LazyGlue model subModel msg subMsg -> ( subModel, Cmd subMsg ) -> ( model, Cmd msg ) -> ( model, Cmd msg )
forceInit glue pair =
    Glue.updateWith glue (\_ -> Tuple.mapFirst Just pair)


{-| Force intialization of module. If model state already exists in the form of `Just model`
this value is overwrite nontherless
-}
forceInitModel : LazyGlue model subModel msg subMsg -> subModel -> model -> model
forceInitModel glue val =
    Glue.updateModelWith glue (\_ -> Just val)


{-| Initialize model only when state doesn't already exist (`Nothing` is current value)
-}
ensure : LazyGlue model subModel msg subMsg -> (() -> ( subModel, Cmd subMsg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
ensure ((Glue.Internal.Glue rec) as glue) f ( model, cmd ) =
    case rec.get model of
        Just v ->
            ( model, cmd )

        Nothing ->
            forceInit glue (f ()) ( model, cmd )


{-| Initialize model only when state doesn't already exist (`Nothing` is current value)
-}
ensureModel : LazyGlue model subModel msg subMsg -> (() -> subModel) -> model -> model
ensureModel ((Glue.Internal.Glue rec) as glue) f model =
    case rec.get model of
        Just v ->
            model

        Nothing ->
            forceInitModel glue (f ()) model


{-| Like `Glue.update` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
In cases where update should be forcing intialization use this in
conjuction with [`ensure`](#ensure)
-}
update : LazyGlue model subModel msg subMsg -> (a -> subModel -> ( subModel, Cmd subMsg )) -> a -> ( model, Cmd msg ) -> ( model, Cmd msg )
update glue f =
    Glue.update glue (\a -> patch (f a))


{-| Like `Glue.updateModel` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
In cases where update should be forcing intialization use this in
conjuction with [`ensureModel`](#ensureModel)
-}
updateModel : LazyGlue model subModel msg subMsg -> (a -> subModel -> subModel) -> a -> model -> model
updateModel glue f =
    Glue.updateModel glue (\a -> Maybe.map (f a))


{-| Like `Glue.updateWith` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
In cases where update should be forcing intialization use this in
conjuction with [`ensure`](#ensure)
-}
updateWith : LazyGlue model subModel msg subMsg -> (subModel -> ( subModel, Cmd subMsg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateWith glue f =
    Glue.updateWith glue (patch f)


{-| Like `Glue.updateModelWith` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
In cases where update should be forcing intialization use this in
conjuction with [`ensureModel`](#ensureModel)
-}
updateModelWith : LazyGlue model subModel msg subMsg -> (subModel -> subModel) -> model -> model
updateModelWith glue f =
    Glue.updateModelWith glue (Maybe.map f)


{-| Like `Glue.triger` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
In cases where update should be forcing intialization use this in
conjuction with [`ensure`](#ensure)
-}
trigger : LazyGlue model subModel msg subMsg -> (subModel -> Cmd subMsg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
trigger glue fc =
    Glue.trigger glue (Maybe.withDefault Cmd.none << Maybe.map fc)


{-| Like `Glue.subscriptions` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
-}
subscriptions : LazyGlue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptions glue f =
    (\m -> Maybe.withDefault Sub.none (Maybe.map f m))
        |> Glue.subscriptions glue


{-| Like `Glue.subscriptionsWhen` but for `LazyGlue` variant.
Update is called only when there model is already initialized.
-}
subscriptionsWhen : (model -> Bool) -> LazyGlue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptionsWhen predicate glue f =
    (\m -> Maybe.withDefault Sub.none (Maybe.map f m))
        |> Glue.subscriptionsWhen predicate glue


{-| Similar to `Glue.view` but forces user to handle `Nothing` case
because API of this module can't really guarantee view won't be called
with uninitialized Model.
-}
view : LazyGlue model subModel msg subMsg -> (subModel -> Html subMsg) -> model -> Maybe (Html msg)
view (Glue.Internal.Glue rec) v model =
    Maybe.map (Html.map rec.msg << v) <| rec.get model



-- HELPERS


patch : (subModel -> ( subModel, Cmd subCmd )) -> (Maybe subModel -> ( Maybe subModel, Cmd subCmd ))
patch f =
    Maybe.withDefault ( Nothing, Cmd.none ) << Maybe.map (Tuple.mapFirst Just << f)
