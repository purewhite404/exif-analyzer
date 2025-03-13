port module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, text, img, div)
import Html.Attributes exposing (id, src, width, class)
import Html.Events exposing (onClick)
import Task
import Json.Decode as Decode exposing (Decoder, int, float, string)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Decode as Decode



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
port getExif : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
  { image : Maybe String
  , exif : Maybe EXIF
  }

type alias EXIF =
  { make : String
  , model : String
  , iso : Int
  , fNumber : Float
  , exposureTime : Float
  , focalLength : Float
  , exposureProgram : String
  , exposureBias : Float
  , date : String
  , lens : String
  }

exifDecoder : Decoder EXIF
exifDecoder =
  Decode.succeed EXIF
    |> required "Make" string
    |> required "Model" string
    |> required "ISOSpeedRatings" int
    |> required "FNumber" float
    |> required "ExposureTime" float
    |> required "FocalLength" float
    |> required "ExposureProgram" string
    |> required "ExposureBias" float
    |> required "DateTime" string
    |> optional "undefined" string ""


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing Nothing,  Cmd.none )

initialEXIF : EXIF
initialEXIF =
  { make = ""
  , model = ""
  , iso = 0
  , fNumber = 0
  , exposureTime = 0
  , focalLength = 0
  , exposureProgram = ""
  , exposureBias = 0
  , date = ""
  , lens = ""
  }



-- UPDATE


type Msg
  = ImageRequested
  | ImageSelected File
  | ImageLoaded String
  | GetExif String


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

    GetExif exifjson ->
      case Decode.decodeString exifDecoder exifjson of
        Ok exif ->
          ( { model | exif = Just exif }
          , Cmd.none )
        Err _ ->
          ( model, Cmd.none )



-- VIEW



view : Model -> Html Msg
view model =
  case (model.image, model.exif) of
    (Just i, Just e) ->
      div []
        [ img [ id "picture", src i, width 800 ] []
        , div [ class "description" ]
          [ div [ class "exifcell left" ]
            [ div [ class "exifelement" ] [ text <| e.make ]
            ]
          , div [ class "exifcell right" ]
            [ div [ class "exifelement" ] [ text <| String.fromFloat e.focalLength ++ "mm" ]
            , div [ class "exifelement" ] [ text <| "F" ++ String.fromFloat e.fNumber ]
            , div [ class "exifelement" ] [ text <| shutterspeed e.exposureTime ]
            , div [ class "exifelement" ] [ text <| "ISO" ++ String.fromInt e.iso ]
            ]
          , div [ class "exifcell left" ]
            [ div [ class "exifelement" ] [ text <| e.model ]
            , div [ class "exifelement" ]
              [ text <| if String.isEmpty e.lens then "" else "/ " ++ e.lens ]
            ]
          , div [ class "exifcell right" ]
            [ div [ class "exifelement" ] [ text <| e.exposureProgram ]
            , div [ class "exifelement" ]
              [ text <| (if e.exposureBias > 0 then "+" else "") ++ String.fromFloat e.exposureBias ++ "EV" ]
            , div [ class "exifelement" ] [ text <| showdate e.date ]
            ]
          ]
        , button [ onClick ImageRequested ] [ text "Upload image" ]
        ]

    (Just i, Nothing) ->
      div []
        [ img [ id "picture", src i, width 800 ] []
        , div [ class "description" ]
          [ div [ class "exifcell left" ]
            [ div [ class "exifelement" ] [ text <| "No exif" ]
            ]
          ]
        , button [ onClick ImageRequested ] [ text "Upload image" ]
        ]

    _ ->
      button [ onClick ImageRequested ] [ text "Upload image" ]

shutterspeed : Float -> String
shutterspeed exposureTime =
  if exposureTime < 1
  then "1/" ++ (String.fromInt <| round (1/exposureTime)) ++ "s"
  else String.fromFloat exposureTime ++ "s"

showdate : String -> String
showdate d =
  let
    date = d 
      |> String.left 10
      |> String.replace ":" "/"
    time = String.right 9 d
  in
  date ++ time

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  getExif GetExif