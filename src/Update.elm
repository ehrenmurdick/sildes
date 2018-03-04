module Update exposing (..)

import Types exposing (Model(..), Msg(..), Slide)
import Network exposing (getSlides)
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
        -- bind message only
        >>- makeRequests
        >>- traceMessage
        -- update model only
        >>$ moveAround
        >>$ setSlides
        -- bind everything
        >>= traceModel "update done"
        --
        |> runUpdate


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


setSlides : Msg -> Model -> Model
setSlides msg model =
    case msg of
        GetSlides (Ok slides) ->
            case slides of
                x :: xs ->
                    (Model x xs [])

                _ ->
                    model

        GetSlides (Err msg) ->
            (Model (Slide "Error" "Error fetching slides") [] [])

        _ ->
            model
