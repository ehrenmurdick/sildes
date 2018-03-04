module Monad exposing (..)

import Ports exposing (log)


type alias Context msg model =
    ( model, msg, Cmd msg )


type alias UpdateFunc msg model =
    msg -> model -> model


fmap : UpdateFunc msg model -> Context msg model -> Context msg model
fmap f c =
    let
        ( mod, msg, cmd ) =
            c
    in
        ( f msg mod, msg, cmd )


(>>$) : Context msg model -> UpdateFunc msg model -> Context msg model
(>>$) m f =
    fmap f m


bindMessage : Context msg model -> (msg -> Cmd msg) -> Context msg model
bindMessage m f =
    m >>= \msg model -> ( model, (f msg) )


(>>-) : Context msg model -> (msg -> Cmd msg) -> Context msg model
(>>-) =
    bindMessage


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


traceModel : String -> msg -> model -> ( model, Cmd msg )
traceModel str msg model =
    ( model
    , Cmd.batch
        [ log (toString model)
        , log str
        ]
    )


traceMessage : msg -> Cmd msg
traceMessage msg =
    log (toString msg)


runUpdate : Context msg model -> ( model, Cmd msg )
runUpdate mmc =
    let
        ( mod, msg, cmd ) =
            mmc
    in
        ( mod, cmd )
