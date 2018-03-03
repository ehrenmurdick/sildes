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


type Model
    = Model Slide (List Slide) (List Slide)


init : ( Model, Cmd Msg )
init =
    ( (Model
        { title = "Slide one"
        , body = "This is the first slide"
        }
        [ { title = "Hello Slides!"
          , body = "These slides are record types."
          }
        , { title = "Slide two"
          , body = "Not sure how I feel about that."
          }
        ]
        []
      )
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Next
    | Prev


flip : Model -> Model
flip model =
    let
        (Model x n p) =
            model
    in
        (Model x p n)


prevSlide : Model -> Model
prevSlide model =
    model
        |> flip
        |> nextSlide
        |> flip


nextSlide : Model -> Model
nextSlide model =
    let
        (Model current next prev) =
            model
    in
        case next of
            x :: xs ->
                (Model x xs (current :: prev))

            [] ->
                model


lift : (Model -> Model) -> Model -> ( Model, Cmd Msg )
lift f m =
    ( f m, Cmd.none )


(>=) : Model -> (Model -> Model) -> ( Model, Cmd Msg )
(>=) m f =
    (lift f) m


advance : Msg -> Model -> Model
advance msg model =
    case msg of
        Next ->
            nextSlide model

        Prev ->
            prevSlide model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model
        >= advance msg



---- VIEW ----


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
