module Network exposing (..)

import Http
import Json.Decode
    exposing
        ( Decoder
        , field
        , list
        , map2
        , string
        )
import Types exposing (Slide, Msg(..))


getSlides : Cmd Msg
getSlides =
    let
        url =
            "http://localhost:3001/slides"

        request =
            Http.get url decodeSlides
    in
        Http.send GetSlides request


decodeSlides : Decoder (List Slide)
decodeSlides =
    list decodeSlide


decodeSlide : Decoder Slide
decodeSlide =
    map2 Slide
        (field "title" string)
        (field "body" string)
