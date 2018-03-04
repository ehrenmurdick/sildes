module Types exposing (..)

import Http
import Html exposing (Html)


type Slide
    = Slide
        { title : String
        , body : String
        }
    | RenderedSlide
        { title : String
        , body : Html Msg
        }


type alias Model =
    { current : Slide, next : List Slide, prev : List Slide }


type Msg
    = Next
    | Prev
    | Refresh
    | Edit
    | GetSlides (Result Http.Error (List Slide))
