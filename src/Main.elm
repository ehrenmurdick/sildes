module Main exposing (..)

import Html
    exposing
        ( Html
        , a
        , br
        , div
        , h1
        , img
        , input
        , text
        , textarea
        )
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (href, value)
import Network exposing (getSlides)
import Types
    exposing
        ( Slide(..)
        , Model
        , emptyModel
        , Msg(..)
        )
import Update exposing (update)


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getSlides )


renderSlide : Slide -> Html Msg
renderSlide slide =
    case slide of
        RenderedSlide attrs ->
            attrs.renderedBody

        EditableSlide attrs ->
            div []
                [ input [ value attrs.title, onInput SetTitle ] []
                , br [] []
                , div [] [ attrs.renderedBody ]
                , br [] []
                , textarea [ value attrs.body, onInput SetBody ] []
                ]

        Slide _ ->
            text "not done rendering"


slideTitle : Slide -> Html Msg
slideTitle slide =
    case slide of
        EditableSlide attrs ->
            text attrs.title

        RenderedSlide attrs ->
            text attrs.title

        Slide attrs ->
            text attrs.title


editing : Model -> Bool
editing model =
    case model.current of
        EditableSlide _ ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ slideTitle model.current ]
        , div [] [ renderSlide model.current ]
        , a [ href "#", onClick Prev ] [ text "Prev" ]
        , text " "
        , a [ href "#", onClick Refresh ] [ text "Refresh" ]
        , text " "
        , if editing model then
            a [ href "#", onClick Save ] [ text "Save" ]
          else
            a [ href "#", onClick Edit ] [ text "Edit" ]
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
