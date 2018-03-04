module Types exposing (..)

import Http


type alias Slide =
    { title : String
    , body : String
    }


type Model
    = Model Slide (List Slide) (List Slide)


type Msg
    = Next
    | Prev
    | Refresh
    | GetSlides (Result Http.Error (List Slide))
