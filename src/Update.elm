module Update exposing (..)

import Types exposing (Model(..), Msg(..), Slide(..))
import Network exposing (getSlides)
import Markdown exposing (toHtml)
import Monad
    exposing
        ( (>>-)
        , (>>$)
        , (>>=)
        , runUpdate
        , traceMessage
        , traceModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, msg, Cmd.none )
        -- side effects
        >>- makeRequests
        >>- traceMessage
        >>= traceModel "before update"
        -- update model
        >>$ moveAround
        >>$ handleResponses
        >>$ renderCurrentSlide
        --
        >>= traceModel "after update"
        |> runUpdate


renderSlide : Slide -> Slide
renderSlide slide =
    case slide of
        RenderedSlide attrs ->
            (RenderedSlide attrs)

        Slide attrs ->
            (RenderedSlide
                { title = attrs.title
                , body = toHtml [] attrs.body
                }
            )


renderCurrentSlide : Msg -> Model -> Model
renderCurrentSlide _ model =
    let
        (Model slide xs ys) =
            model

        newSlide =
            renderSlide slide
    in
        (Model newSlide xs ys)


flipM : Model -> Model
flipM model =
    let
        (Model x n p) =
            model
    in
        (Model x p n)


prevSlide : Model -> Model
prevSlide =
    flipM >> nextSlide >> flipM


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


moveAround : Msg -> Model -> Model
moveAround msg model =
    case msg of
        Next ->
            nextSlide model

        Prev ->
            prevSlide model

        _ ->
            model


makeRequests : Msg -> Cmd Msg
makeRequests msg =
    case msg of
        Refresh ->
            getSlides

        _ ->
            Cmd.none


handleResponses : Msg -> Model -> Model
handleResponses msg model =
    case msg of
        GetSlides (Ok slides) ->
            case slides of
                x :: xs ->
                    (Model x xs [])

                _ ->
                    model

        GetSlides (Err msg) ->
            (Model (Slide { title = "Error", body = "Error fetching slides" }) [] [])

        _ ->
            model
