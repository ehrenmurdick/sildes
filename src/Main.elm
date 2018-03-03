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


---- MODEL ----


type alias Slide =
    { title : String
    , body : String
    }


type alias Model =
    { current : Slide
    , next : List Slide
    , prev : List Slide
    }


init : ( Model, Cmd Msg )
init =
    ( { next =
            [ { title = "Hello Slides!"
              , body = "These slides are record types."
              }
            , { title = "Slide two"
              , body = "Not sure how I feel about that."
              }
            ]
      , current = { title = "Slide one", body = "" }
      , prev = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Next
    | Prev


prevSlide : Model -> Model
prevSlide model =
    case model.prev of
        x :: xs ->
            { model
                | prev = xs
                , current = x
                , next = model.current :: model.next
            }

        [] ->
            model


nextSlide : Model -> Model
nextSlide model =
    case model.next of
        x :: xs ->
            { model
                | next = xs
                , current = x
                , prev = model.current :: model.prev
            }

        [] ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( nextSlide model, Cmd.none )

        Prev ->
            ( prevSlide model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.current.title ]
        , div [] [ text model.current.body ]
        , a [ href "#", onClick Prev ] [ text "Prev" ]
        , text " "
        , a [ href "#", onClick Next ] [ text "Next" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
