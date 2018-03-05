module Monad exposing (..)

import Ports exposing (log)


(>=>) :
    (msg -> model -> ( model, Cmd msg ))
    -> (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> ( model, Cmd msg )
(>=>) f g =
    \msg model ->
        let
            ( newModel, newCmd ) =
                f msg model

            ( twoModel, twoCmd ) =
                g msg newModel
        in
            ( twoModel, Cmd.batch [ newCmd, twoCmd ] )


sequence : msg -> model -> ( model, Cmd msg )
sequence msg model =
    ( model, Cmd.none )


noCmd : model -> ( model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


traceModel : String -> msg -> model -> ( model, Cmd msg )
traceModel str msg model =
    ( model
    , Cmd.batch
        [ log (toString model)
        , log str
        ]
    )


traceMessage : msg -> model -> ( model, Cmd msg )
traceMessage msg model =
    ( model, log (toString msg) )
