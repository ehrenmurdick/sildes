module Update exposing (..)

import Types exposing (Model, Msg(..), Slide(..))
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
        >>$ handleInputs
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

        EditableSlide attrs ->
            (EditableSlide attrs)

        Slide attrs ->
            (RenderedSlide
                { title = attrs.title
                , body = attrs.body
                , renderedBody = toHtml [] attrs.body
                }
            )


renderCurrentSlide : Msg -> Model -> Model
renderCurrentSlide _ model =
    let
        newSlide =
            renderSlide model.current
    in
        { model | current = newSlide }


flipM : Model -> Model
flipM model =
    { model | next = model.prev, prev = model.next }


prevSlide : Model -> Model
prevSlide =
    flipM >> nextSlide >> flipM


nextSlide : Model -> Model
nextSlide model =
    case model.next of
        x :: xs ->
            { model
                | current = x
                , next = xs
                , prev = model.current :: model.prev
            }

        [] ->
            model


saveSlide : Slide -> Slide
saveSlide slide =
    case slide of
        RenderedSlide _ ->
            slide

        Slide _ ->
            slide

        EditableSlide attrs ->
            Slide attrs


editSlide : Slide -> Slide
editSlide slide =
    case slide of
        RenderedSlide attrs ->
            (EditableSlide
                { title = attrs.title
                , body = attrs.body
                }
            )

        Slide attrs ->
            EditableSlide attrs

        EditableSlide attrs ->
            EditableSlide attrs


handleInputs : Msg -> Model -> Model
handleInputs msg model =
    case msg of
        Next ->
            nextSlide model

        Prev ->
            prevSlide model

        Edit ->
            { model | current = editSlide model.current }

        Save ->
            { model | current = saveSlide model.current }

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
                    { model | current = x, next = xs, prev = [] }

                _ ->
                    model

        GetSlides (Err msg) ->
            { model
                | current = (Slide { title = "Error", body = "Error fetching slides" })
            }

        _ ->
            model
