module Monad exposing (..)

import Ports exposing (log)


fmap : (msg -> model -> model) -> ( model, msg, Cmd msg ) -> ( model, msg, Cmd msg )
fmap f c =
    let
        ( mod, msg, cmd ) =
            c
    in
        ( f msg mod, msg, cmd )


(>>$) : ( model, msg, Cmd msg ) -> (msg -> model -> model) -> ( model, msg, Cmd msg )
(>>$) m f =
    fmap f m


bindMessage : ( model, msg, Cmd msg ) -> (msg -> Cmd msg) -> ( model, msg, Cmd msg )
bindMessage m f =
    m >>= \msg model -> ( model, (f msg) )


(>>-) : ( model, msg, Cmd msg ) -> (msg -> Cmd msg) -> ( model, msg, Cmd msg )
(>>-) =
    bindMessage


bindContext : ( model, msg, Cmd msg ) -> (msg -> model -> ( model, Cmd msg )) -> ( model, msg, Cmd msg )
bindContext m f =
    let
        ( model, msg, cmd ) =
            m

        ( newmodel, newCmd ) =
            f msg model
    in
        ( newmodel, msg, Cmd.batch [ cmd, newCmd ] )


(>>=) : ( model, msg, Cmd msg ) -> (msg -> model -> ( model, Cmd msg )) -> ( model, msg, Cmd msg )
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


runUpdate : ( model, msg, Cmd msg ) -> ( model, Cmd msg )
runUpdate mmc =
    let
        ( mod, msg, cmd ) =
            mmc
    in
        ( mod, cmd )
