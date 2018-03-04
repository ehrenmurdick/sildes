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


type Model
    = Model Slide (List Slide) (List Slide)


type Msg
    = Next
    | Prev
    | Refresh
    | GetSlides (Result Http.Error (List Slide))
