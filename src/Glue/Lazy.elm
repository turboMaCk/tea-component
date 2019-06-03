module Glue.Lazy exposing (LazyGlue)

import Glue exposing (Glue)


type alias LazyGlue model subModel msg subMsg =
    Glue model (Maybe subModel) msg subMsg


initLater : Glue model subModel msg subMsg -> ( Maybe subModel -> a, Cmd msg ) -> ( a, Cmd msg )
initLater _ ( f, cmd ) =
    ( f Nothing, cmd )


forceInit : LazyGlue model subModel msg subMsg -> ( subModel, Cmd subMsg ) -> ( model, Cmd msg ) -> ( model, Cmd msg )
forceInit glue pair =
    Glue.updateWith glue (\_ -> Tuple.mapFirst Just pair)


forceInitModel : LazyGlue model subModel msg subMsg -> subModel -> model -> model
forceInitModel glue val =
    Glue.updateModelWith glue (\_ -> Just val)


update : LazyGlue model subModel msg subMsg -> (a -> subModel -> ( subModel, Cmd subMsg )) -> a -> ( model, Cmd msg ) -> ( model, Cmd msg )
update glue f =
    Glue.update glue (\a -> patch (f a))


updateModel : LazyGlue model subModel msg subMsg -> (a -> subModel -> subModel) -> a -> model -> model
updateModel glue f =
    Glue.updateModel glue (\a -> Maybe.map (f a))


updateWith : LazyGlue model subModel msg subMsg -> (subModel -> ( subModel, Cmd subMsg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateWith glue f =
    Glue.updateWith glue (patch f)


updateWithModel : LazyGlue model subModel msg subMsg -> (subModel -> subModel) -> model -> model
updateWithModel glue f =
    Glue.updateModelWith glue (Maybe.map f)


tigger : LazyGlue model subModel msg subMsg -> (subModel -> Cmd subMsg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
tigger glue fc =
    Glue.trigger glue (Maybe.withDefault Cmd.none << Maybe.map fc)


subscriptions : LazyGlue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptions glue f =
    (\m -> Maybe.withDefault Sub.none (Maybe.map f m))
        |> Glue.subscriptions glue


subscriptionsWhen : (model -> Bool) -> LazyGlue model subModel msg subMsg -> (subModel -> Sub subMsg) -> (model -> Sub msg) -> (model -> Sub msg)
subscriptionsWhen predicate glue f =
    (\m -> Maybe.withDefault Sub.none (Maybe.map f m))
        |> Glue.subscriptionsWhen predicate glue


patch : (subModel -> ( subModel, Cmd subCmd )) -> (Maybe subModel -> ( Maybe subModel, Cmd subCmd ))
patch f =
    Maybe.withDefault ( Nothing, Cmd.none ) << Maybe.map (Tuple.mapFirst Just << f)
