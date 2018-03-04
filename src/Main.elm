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


flip : Model -> Model
flip model =
    let
        (Model x n p) =
            model
    in
        (Model x p n)


prevSlide : Model -> Model
prevSlide model =
    model
        |> flip
        |> nextSlide
        |> flip


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


lift : (Model -> Model) -> Model -> ( Model, Cmd Msg )
lift f m =
    ( f m, Cmd.none )


pure : Model -> ( Model, Cmd Msg )
pure m =
    ( m, Cmd.none )


(->-) : ( Model, Cmd Msg ) -> Cmd Msg -> ( Model, Cmd Msg )
(->-) m cmd =
    m >>= \model -> ( model, cmd )


(>>-) : ( Model, Cmd Msg ) -> (Model -> Model) -> ( Model, Cmd Msg )
(>>-) m f =
    m >>= (lift f)


(>>=) : ( Model, Cmd Msg ) -> (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
(>>=) m f =
    let
        ( model, cmd ) =
            m

        ( newModel, newCmd ) =
            f model
    in
        ( newModel, Cmd.batch [ cmd, newCmd ] )


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
            (Model (Slide "Error" (toString msg)) [] [])

        _ ->
            model


logModel : Model -> ( Model, Cmd Msg )
logModel model =
    ( model, log (toString model) )


mempty : Model
mempty =
    Model (Slide "" "") [] []


logMessage : Msg -> Cmd Msg
logMessage msg =
    log (toString msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pure model
        ->- makeRequests msg
        ->- logMessage msg
        >>- moveAround msg
        >>- setSlides msg
        >>= logModel



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
