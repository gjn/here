module Main exposing (main)

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input

import Html exposing (Html)

main =
    Browser.sandbox { init = init, update = update, view = view }


-- DEFINITIONS

type alias Information =
  { topic : String, layer : String, pre : String, post : String, value : String }


-- MODEL

type alias Model =
  {
    topics : List String
  , activeTopic : String
  , informations : List Information
  }


init : Model
init = 
  {
    topics =
      [ "Generell"
      , "Verkehr"
      , "Energie"
      , "Gefahr"
      , "Natur"
      , "Interessantes"
      , "Besitz"
      ]
  , activeTopic = "Generell"
  , informations =
      [ { topic = "Generell", layer = "ch.swisstopo.bogus", pre = "Ihr Standort ist ", post = " vom naechsten entfernt", value = "5 km" }
      , { topic = "Generell", layer = "ch.swisstopo.bogus", pre = "Ihr Standort ist ", post = " vom naechsten entfernt", value = "5 km" }
      , { topic = "Generell", layer = "ch.swisstopo.bogus", pre = "Ihr Standort ist ", post = " vom naechsten entfernt", value = "5 km" }
      ]
  }

-- UPDATE

type Msg =
  ChangeTopic String

update msg model =
  case msg of
    ChangeTopic newtopic ->
      {model | activeTopic = newtopic }


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


mainPanel : String -> List Information -> Element msg
mainPanel topic informations =
    let
        header =
            row
                [ width fill
                , paddingXY 20 5
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 200 200 200
                ]
                [ el [] <| text ("- " ++ topic)
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
                List.map informationEntry informations

        footer =
            el [ alignBottom, padding 20, width fill ] <|
                row
                    [ spacingXY 2 0
                    , width fill
                    , Border.width 2
                    , Border.rounded 4
                    , Border.color <| rgb255 200 200 200
                    ]
                    [ el
                        [ padding 5
                        , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
                        , Border.color <| rgb255 200 200 200
                        , mouseOver [ Background.color <| rgb255 86 182 139 ]
                        ]
                      <|
                        text "+"
                    , el [ Background.color <| rgb255 255 255 255 ] none
                    ]
    in
    column [ height fill, width <| fillPortion 5 ]
        [ header
        , informationPanel
        , footer
        ]


view model =
    layout [ height fill ] <|
        row [ height fill, width fill ]
            [ topicPanel model.topics model.activeTopic
            , mainPanel model.activeTopic model.informations
            ]

