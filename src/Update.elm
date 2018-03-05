module Update exposing (..)

import Html exposing (text)
import Types
    exposing
        ( Model
        , Clicks(..)
        , FormInput(..)
        , Msg(..)
        , Slide(..)
        )
import Network exposing (getSlides)
import Markdown exposing (toHtml)
import Monad
    exposing
        ( (>=>)
        , traceMessage
        , sequence
        , noCmd
        , traceModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    sequence
        >=> traceMessage
        >=> traceModel "before update"
        >=> selectFormInput handleFormInput
        >=> selectClicks handleClicks
        >=> handleResponses
        >=> renderCurrentSlide
        >=> traceModel "after update"


selectClicks : (Clicks -> Model -> ( Model, Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
selectClicks f msg model =
    case msg of
        Clicks ip ->
            f ip model

        _ ->
            ( model, Cmd.none )


selectFormInput : (FormInput -> Model -> ( Model, Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
selectFormInput f msg model =
    case msg of
        FormInput ip ->
            f ip model

        _ ->
            ( model, Cmd.none )


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


renderCurrentSlide : Msg -> Model -> ( Model, Cmd Msg )
renderCurrentSlide _ model =
    let
        newSlide =
            renderSlide model.current
    in
        ( { model | current = newSlide }, Cmd.none )


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
            RenderedSlide attrs


editSlide : Slide -> Slide
editSlide slide =
    case slide of
        RenderedSlide attrs ->
            (EditableSlide
                { title = attrs.title
                , body = attrs.body
                , renderedBody = attrs.renderedBody
                }
            )

        Slide attrs ->
            EditableSlide
                { title = attrs.title
                , renderedBody = text ""
                , body = attrs.body
                }

        EditableSlide attrs ->
            EditableSlide attrs


updateBody : String -> Slide -> Slide
updateBody body slide =
    case slide of
        EditableSlide attrs ->
            EditableSlide { attrs | body = body, renderedBody = toHtml [] body }

        _ ->
            EditableSlide { title = "", body = "", renderedBody = text "" }


updateTitle : String -> Slide -> Slide
updateTitle title slide =
    case slide of
        EditableSlide attrs ->
            EditableSlide { attrs | title = title }

        _ ->
            EditableSlide { title = "", body = "", renderedBody = text "" }


handleClicks : Clicks -> Model -> ( Model, Cmd Msg )
handleClicks msg model =
    case msg of
        Next ->
            noCmd <| nextSlide model

        Edit ->
            noCmd { model | current = editSlide model.current }

        Prev ->
            noCmd <| prevSlide model

        Refresh ->
            ( model, getSlides )


handleFormInput : FormInput -> Model -> ( Model, Cmd Msg )
handleFormInput msg model =
    case msg of
        Save ->
            noCmd { model | current = saveSlide model.current }

        SetTitle title ->
            noCmd { model | current = updateTitle title model.current }

        SetBody body ->
            noCmd { model | current = updateBody body model.current }


handleResponses : Msg -> Model -> ( Model, Cmd Msg )
handleResponses msg model =
    ( (case msg of
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
      )
    , Cmd.none
    )
