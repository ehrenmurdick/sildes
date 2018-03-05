module Monad exposing (..)

import Ports exposing (log)


type alias Context msg model =
    ( model, msg, Cmd msg )


bindContext : Context msg model -> (msg -> model -> ( model, Cmd msg )) -> Context msg model
bindContext m f =
    let
        ( model, msg, cmd ) =
            m

        ( newmodel, newCmd ) =
            f msg model
    in
        ( newmodel, msg, Cmd.batch [ cmd, newCmd ] )


(>>=) : Context msg model -> (msg -> model -> ( model, Cmd msg )) -> Context msg model
(>>=) =
    bindContext


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


runUpdate : Context msg model -> ( model, Cmd msg )
runUpdate mmc =
    let
        ( mod, msg, cmd ) =
            mmc
    in
        ( mod, cmd )
