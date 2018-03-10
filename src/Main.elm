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
        , FormInput(..)
        , Clicks(..)
        )
import Update exposing (update)
import Zipper.List as Z


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getSlides )


renderSlideForm : Slide -> Html FormInput
renderSlideForm slide =
    case slide of
        EditableSlide attrs ->
            div []
                [ input [ value attrs.title, onInput SetTitle ] []
                , br [] []
                , textarea [ value attrs.body, onInput SetBody ] []
                , br [] []
                , a [ href "#", onClick Save ] [ text "Save" ]
                ]

        _ ->
            text ""


renderSlide : Slide -> Html Clicks
renderSlide slide =
    case slide of
        RenderedSlide attrs ->
            attrs.renderedBody

        EditableSlide attrs ->
            attrs.renderedBody

        Slide _ ->
            text "not done rendering"


slideTitle : Slide -> Html Clicks
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
    case Z.current model of
        EditableSlide _ ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div []
        [ Html.map Clicks <|
            div []
                [ h1 [] [ slideTitle (Z.current model) ]
                , div [] [ renderSlide (Z.current model) ]
                , a [ href "#", onClick Prev ] [ text "Prev" ]
                , text " "
                , a [ href "#", onClick Refresh ] [ text "Refresh" ]
                , text " "
                , if not (editing model) then
                    a [ href "#", onClick Edit ] [ text "Edit" ]
                  else
                    text ""

                -- a [ href "#", onClick Save ] [ text "Save" ]
                , text " "
                , a [ href "#", onClick Next ] [ text "Next" ]
                ]
        , Html.map FormInput <|
            div [] [ renderSlideForm (Z.current model) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
