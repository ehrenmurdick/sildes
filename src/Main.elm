module Main exposing (..)

import Html
    exposing
        ( Html
        , a
        , div
        , h1
        , img
        , text
        )
import Html.Events exposing (onClick)
import Html.Attributes exposing (href)
import Network exposing (getSlides)
import Types
    exposing
        ( Slide
        , Model(..)
        , Msg(..)
        )
import Update exposing (update)


init : ( Model, Cmd Msg )
init =
    ( (Model (Slide "" "") [] []), getSlides )


view : Model -> Html Msg
view model =
    let
        (Model current _ _) =
            model
    in
        div []
            [ h1 [] [ text current.title ]
            , div [] [ text current.body ]
            , a [ href "#", onClick Prev ] [ text "Prev" ]
            , text " "
            , a [ href "#", onClick Refresh ] [ text "Refresh" ]
            , text " "
            , a [ href "#", onClick Next ] [ text "Next" ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
