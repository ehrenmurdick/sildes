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
        , renderedBody : Html Clicks
        }
    | EditableSlide
        { title : String
        , body : String
        , renderedBody : Html Clicks
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


type FormInput
    = SetBody String
    | SetTitle String
    | Save


type Clicks
    = Next
    | Edit
    | Prev
    | Refresh


type Msg
    = Clicks Clicks
    | FormInput FormInput
    | GetSlides (Result Http.Error (List Slide))
