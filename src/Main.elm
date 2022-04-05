module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getAdvice )


type Msg
    = GetAdvice
    | GotAdvice (Result Http.Error String)


adviceDecoder : D.Decoder String
adviceDecoder =
    D.field "slip" (D.field "advice" D.string)


getAdvice : Cmd Msg
getAdvice =
    let
        resolver =
            Http.stringResolver <|
                \response ->
                    case response of
                        Http.BadUrl_ url ->
                            Err (Http.BadUrl url)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body ->
                            case D.decodeString adviceDecoder body of
                                Ok value ->
                                    Ok value

                                Err err ->
                                    Err (Http.BadBody (D.errorToString err))

        getNewAdvice currentTime =
            Http.task
                { method = "GET"
                , headers = []
                , url = "https://api.adviceslip.com/advice?t=" ++ String.fromInt (Time.posixToMillis currentTime)
                , body = Http.emptyBody
                , resolver = resolver
                , timeout = Nothing
                }

        makeRequest =
            Time.now
                |> Task.andThen getNewAdvice
    in
    Task.attempt GotAdvice makeRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAdvice ->
            ( model, getAdvice )

        GotAdvice result ->
            case result of
                Ok advice ->
                    ( Success advice, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            div []
                [ text "I can’t seem to come up with any good advice at the moment..."
                , button [ onClick GetAdvice ] [ text "Try again" ]
                ]

        Loading ->
            text "I’m thinking..."

        Success advice ->
            div []
                [ text advice
                , button
                    [ onClick GetAdvice
                    , HA.style "display" "block"
                    ]
                    [ text "Get more" ]
                ]
