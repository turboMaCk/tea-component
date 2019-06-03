module Glue.Internal exposing (Glue(..))


type Glue model subModel msg subMsg
    = Glue
        { msg : subMsg -> msg
        , get : model -> subModel
        , set : subModel -> model -> model
        }
