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
        , renderedBody : Html Msg
        }
    | EditableSlide
        { title : String
        , body : String
        , renderedBody : Html Msg
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


type Msg
    = Next
    | Prev
    | Refresh
    | Edit
    | Save
    | SetTitle String
    | SetBody String
    | GetSlides (Result Http.Error (List Slide))
