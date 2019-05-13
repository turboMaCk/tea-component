module Subscriptions.Moves exposing (Model, Msg, Position, init, positionDecoder, subscriptions, update, view)

{-| This demonstrates how subscription composition works with gluing.

Please be aware that this example was made just for purposes of this demonstration.

-}

import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onMouseMove <| Decode.map Moved positionDecoder



-- Model


type alias Position =
    { x : Int, y : Int }


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 (\x y -> { x = x, y = y })
        (Decode.field "x" Decode.int)
        (Decode.field "x" Decode.int)


type alias Model =
    Position


init : ( Model, Cmd Msg )
init =
    ( { x = 0, y = 0 }, Cmd.none )



-- Update


type Msg
    = Moved Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Moved position ->
            ( position
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    Html.text <| "x: " ++ String.fromInt model.x ++ " y: " ++ String.fromInt model.y
