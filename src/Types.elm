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
        , body : String
        , renderedBody : Html Input
        }
    | EditableSlide
        { title : String
        , body : String
        , renderedBody : Html Input
        }


type alias Model =
    { current : Slide
    , next : List Slide
    , prev : List Slide
    }


emptyModel : Model
emptyModel =
    { current = (Slide { title = "", body = "" })
    , next = []
    , prev = []
    }


type Input
    = Next
    | Prev
    | Refresh
    | Edit
    | Save
    | SetBody String
    | SetTitle String


type Msg
    = Input Input
    | GetSlides (Result Http.Error (List Slide))
