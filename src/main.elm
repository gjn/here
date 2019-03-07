module Main exposing (main)

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input

import Html exposing (Html)
import Http
import Json.Decode  exposing (Decoder, index, field, string, float)

main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- DEFINITIONS

type alias Information =
  { topic : String, layer : String, pre : String, post : String, value : String }

type alias TAddress =
  { label : String, x : Float, y : Float }

type alias TIdentify =
  { label: String, distance : Float }

type Response
  = Address TAddress
  | Identify TIdentify

-- MODEL

type HttpRequestState
  = Idle
  | Loading
  | Failure
  | Success Response

type alias Model =
  {
    topics : List String
  , activeTopic : String
  , informations : List Information
  , searchString : String
  , searchState : HttpRequestState
  }


init : () -> (Model, Cmd Msg)
init _ = 
  ({
    topics =
      [ "Verkehr"
      , "Energie"
      , "Gefahr"
      , "Natur"
      , "Interessantes"
      , "Besitz"
      ]
  , activeTopic = "Verkehr"
  , informations =
      [ { topic = "Verkehr", layer = "ch.swisstopo.bogus", pre = "Ihr Standort ist ", post = " vom naechsten entfernt", value = "5 km" }
      , { topic = "Verkehr", layer = "ch.swisstopo.bogus", pre = "Ihr Standort ist ", post = " vom naechsten entfernt", value = "5 km" }
      , { topic = "Verkehr", layer = "ch.swisstopo.bogus", pre = "Ihr Standort ist ", post = " vom naechsten entfernt", value = "5 km" }
      ]
  , searchString = ""
  , searchState = Idle
  }
  , Cmd.none)

-- UPDATE

getStateText : HttpRequestState -> String
getStateText state =
  case state of
    Idle ->
      ""

    Loading ->
      "Loading..."

    Failure ->
      "Address search didn't find any results or failed"

    Success resp ->
      case resp of
        Address address ->
          (address.label ++ " " ++ String.fromFloat address.x ++ "/" ++ String.fromFloat address.y)

        Identify ident ->
          ""

addressDecoder : Decoder TAddress
addressDecoder =
  Json.Decode.map3 TAddress
    (field "results" (index 0 (field "attrs" (field "label" string))))
    (field "results" (index 0 (field "attrs" (field "x" float))))
    (field "results" (index 0 (field "attrs" (field "y" float))))

getAddress : String -> Cmd Msg
getAddress searchstring =
  Http.get
    { url =
      "https://api3.geo.admin.ch/rest/services/all/SearchServer?sr=2056&searchText=" ++ searchstring ++ "&lang=de&type=locations"
    , expect = Http.expectJson GotSearch addressDecoder
    }

type Msg =
  ChangeTopic String
  | UpdateSearch String
  | GotSearch (Result Http.Error TAddress)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTopic newtopic ->
      ({model | activeTopic = newtopic }, Cmd.none)

    UpdateSearch searchstring ->
      ({model | searchString = searchstring, searchState = Loading}, getAddress searchstring)

    GotSearch result ->
      case result of
        Ok  resultJson ->
          ({model | searchState = Success (Address resultJson)}, Cmd.none)

        Err _ ->
          ({model | searchState = Failure}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

topicPanel : List String -> String -> Element Msg
topicPanel topics activeTopic =
    let
        activeTopicAttrs =
            [ Background.color <| rgb255 117 179 201, Font.bold ]

        topicAttrs topic =
            [ paddingXY 15 5, onClick (ChangeTopic topic), width fill ]

        topicEl topic =
            el
                (if topic == activeTopic then
                    activeTopicAttrs ++ (topicAttrs topic)

                 else
                    topicAttrs topic
                )
            <|         
                text ("- " ++ topic)
    in
    column
        [ height fill
        , width <| fillPortion 1
        , paddingXY 0 10
        , scrollbars
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        List.map topicEl topics


mainPanel : Model -> Element Msg
mainPanel model =
    let
        header =
            row
                [ width fill
                , padding 5
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 200 200 200
                ]
                [ el [ padding 5 ] <| text ("- " ++ model.activeTopic)
                , Input.text
                  [
                    padding 5
                  ]
                  { label = Input.labelHidden model.activeTopic
                  , onChange = \new -> UpdateSearch new
                  , placeholder = Just (Input.placeholder [] (text "Geben Sie eine Adresse ein"))
                  , text = model.searchString
                  }
                , Input.button
                    [ padding 5
                    , alignRight
                    , Border.width 1
                    , Border.rounded 3
                    , Border.color <| rgb255 200 200 200
                    ]
                    { onPress = Nothing
                    , label = text "Search"
                    }
                ]

        informationEntry information =
            column [ width fill, spacingXY 0 5 ]
                [ row [ spacingXY 10 0 ]
                    [ el [ Font.bold ] <| text information.layer, text information.pre, text information.value ]
                , paragraph [] [ text information.post ]
                ]

        informationPanel =
            column [ padding 10, spacingXY 0 20, scrollbarY ] <|
                List.map informationEntry model.informations

        searchResultPanel =
            el [ padding 5 ] <| text (getStateText model.searchState)
    in
    column [ height fill, width <| fillPortion 5 ]
        [ header
        , searchResultPanel
        , informationPanel
        ]


view model =
    layout [ height fill ] <|
        row [ height fill, width fill ]
            [ topicPanel model.topics model.activeTopic
            , mainPanel model
            ]

