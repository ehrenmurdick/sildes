module Types exposing (..)

import Http
import Html exposing (Html)
import Zipper.List as Z


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
    Z.Zipper Slide


emptyModel : Model
emptyModel =
    Z.zipper (Slide { title = "", body = "" }) []


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
