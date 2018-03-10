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
import Zipper.List as Z
import Monad
    exposing
        ( (>=>)
        , noCmd
        , sequence
        , trace
        , traceMessage
        , traceModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    sequence
        >=> trace "lets get this party started"
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
    ( Z.update renderSlide model, Cmd.none )


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
            noCmd <| Z.next model

        Edit ->
            noCmd <| Z.update editSlide model

        Prev ->
            noCmd <| Z.prev model

        Refresh ->
            ( model, getSlides )


handleFormInput : FormInput -> Model -> ( Model, Cmd Msg )
handleFormInput msg model =
    case msg of
        Save ->
            noCmd (Z.update saveSlide model)

        SetTitle title ->
            noCmd (Z.update (updateTitle title) model)

        SetBody body ->
            noCmd (Z.update (updateBody body) model)


handleResponses : Msg -> Model -> ( Model, Cmd Msg )
handleResponses msg model =
    ( (case msg of
        GetSlides (Ok slides) ->
            case slides of
                x :: xs ->
                    Z.zipper x xs

                _ ->
                    model

        GetSlides (Err msg) ->
            Z.zipper (Slide { title = "Error", body = "Error fetching slides" }) []

        _ ->
            model
      )
    , Cmd.none
    )
