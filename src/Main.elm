module Main exposing (..)

import Browser
import Html exposing (Html, text, button, pre)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D

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


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading, getAdvice )


type Msg
  = GetAdvice
  | GotAdvice (Result Http.Error String)


getAdvice : Cmd Msg
getAdvice =
  Http.request
    { method = "GET"
    , headers = []
    , url = "https://api.adviceslip.com/advice"
    , body = Http.emptyBody
    , expect = Http.expectJson GotAdvice (D.field "slip" (D.field "advice" D.string))
    , timeout = Nothing
    , tracker = Nothing
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetAdvice ->
      (model, getAdvice)

    GotAdvice result ->
      case result of
        Ok advice ->
          (Success advice, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      Html.div []
        [ text "I can’t seem to come up with any good advice at the moment..."
        , button [ onClick GetAdvice ] [ text "Try again" ]
        ]

    Loading ->
      text "I’m thinking..."

    Success advice ->
      pre []
        [ text advice
        , button [ onClick GetAdvice ] [ text "Get more" ]
        ]
