port module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, text, img, div)
import Html.Attributes exposing (id, src, width, class)
import Html.Events exposing (onClick)
import Task



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- PORTS



port analyzeExif : String -> Cmd msg



-- MODEL


type alias Model =
  { image : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
  = ImageRequested
  | ImageSelected File
  | ImageLoaded String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ImageRequested ->
      ( model
      , Select.file ["image/jpeg"] ImageSelected
      )


    ImageSelected file ->
      ( model
      , Task.perform ImageLoaded (File.toUrl file)
      )


    ImageLoaded content ->
      ( { model | image = Just content }
      , analyzeExif content
      )



-- VIEW



view : Model -> Html Msg
view model =
  case model.image of
    Nothing ->
      button [ onClick ImageRequested ] [ text "Upload image" ]

    Just content ->
      div []
        [ img [ id "picture", src content, width 400 ] []
        , div [ class "description" ]
          [ div [] [ text "ISO 200" ]
          , div [] [ text "OLYMPUS" ]
          ]
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none