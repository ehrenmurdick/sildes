module Main exposing (..)

import Html
    exposing
        ( Html
        , a
        , div
        , h1
        , img
        , text
        )
import Html.Events exposing (onClick)
import Html.Attributes exposing (href)
import Http
import Json.Decode
    exposing
        ( Decoder
        , field
        , list
        , map2
        , string
        )
import Ports exposing (log)


---- MODEL ----


type alias Slide =
    { title : String
    , body : String
    }


type Model
    = Model Slide (List Slide) (List Slide)


init : ( Model, Cmd Msg )
init =
    ( (Model
        (Slide "" "")
        []
        []
      )
    , getSlides
    )



---- UPDATE ----


type Msg
    = Next
    | Prev
    | Refresh
    | GetSlides (Result Http.Error (List Slide))


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


fmap : (Msg -> Model -> Model) -> ( Model, Msg, Cmd Msg ) -> ( Model, Msg, Cmd Msg )
fmap f c =
    let
        ( mod, msg, cmd ) =
            c
    in
        ( f msg mod, msg, cmd )


(>$>) : ( Model, Msg, Cmd Msg ) -> (Msg -> Model -> Model) -> ( Model, Msg, Cmd Msg )
(>$>) m f =
    fmap f m


(->-) : ( Model, Msg, Cmd Msg ) -> (Msg -> Cmd Msg) -> ( Model, Msg, Cmd Msg )
(->-) m f =
    m >>= \msg model -> ( model, (f msg) )


(>>=) : ( Model, Msg, Cmd Msg ) -> (Msg -> Model -> ( Model, Cmd Msg )) -> ( Model, Msg, Cmd Msg )
(>>=) m f =
    let
        ( model, msg, cmd ) =
            m

        ( newModel, newCmd ) =
            f msg model
    in
        ( newModel, msg, Cmd.batch [ cmd, newCmd ] )


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


traceModel : String -> Msg -> Model -> ( Model, Cmd Msg )
traceModel str msg model =
    ( model
    , Cmd.batch
        [ log (toString model)
        , log str
        ]
    )


traceMessage : Msg -> Cmd Msg
traceMessage msg =
    log (toString msg)


runUpdate : ( Model, Msg, Cmd Msg ) -> ( Model, Cmd Msg )
runUpdate mmc =
    let
        ( mod, msg, cmd ) =
            mmc
    in
        ( mod, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, msg, Cmd.none )
        ->- makeRequests
        ->- traceMessage
        --
        >$> moveAround
        >$> setSlides
        --
        >>= traceModel "after render"
        --
        |> runUpdate



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        (Model current _ _) =
            model
    in
        div []
            [ h1 [] [ text current.title ]
            , div [] [ text current.body ]
            , a [ href "#", onClick Prev ] [ text "Prev" ]
            , text " "
            , a [ href "#", onClick Refresh ] [ text "Refresh" ]
            , text " "
            , a [ href "#", onClick Next ] [ text "Next" ]
            ]



---- HTTP ----


getSlides : Cmd Msg
getSlides =
    let
        url =
            "http://localhost:3001/slides"

        request =
            Http.get url decodeSlides
    in
        Http.send GetSlides request


decodeSlides : Decoder (List Slide)
decodeSlides =
    list decodeSlide


decodeSlide : Decoder Slide
decodeSlide =
    map2 Slide
        (field "title" string)
        (field "body" string)



----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
