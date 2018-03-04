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
        ( Slide(..)
        , Model
        , Msg(..)
        )
import Update exposing (update)


init : ( Model, Cmd Msg )
init =
    ( (Model (Slide { title = "", body = "" }) [] []), getSlides )


renderSlide : Slide -> Html Msg
renderSlide slide =
    case slide of
        RenderedSlide attrs ->
            attrs.body

        Slide _ ->
            text "not done rendering"


slideTitle : Slide -> Html Msg
slideTitle slide =
    case slide of
        RenderedSlide attrs ->
            text attrs.title

        Slide attrs ->
            text attrs.title


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ slideTitle model.current ]
        , div [] [ renderSlide model.current ]
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
