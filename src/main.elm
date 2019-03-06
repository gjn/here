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
  , searchString : String
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
  , searchString = ""
  }

-- UPDATE

search: Model -> String -> Model
search model string =
  {model | searchString = string}


type Msg =
  ChangeTopic String
  | UpdateSearch String

update msg model =
  case msg of
    ChangeTopic newtopic ->
      {model | activeTopic = newtopic }

    UpdateSearch searchstring ->
      search model searchstring


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


mainPanel : String -> List Information -> String -> Element Msg
mainPanel topic informations searchstring =
    let
        header =
            row
                [ width fill
                , padding 5
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 200 200 200
                ]
                [ el [ padding 5 ] <| text ("- " ++ topic)
                , Input.text
                  [
                    padding 5
                  ]
                  { label = Input.labelHidden topic
                  , onChange = \new -> UpdateSearch new
                  , placeholder = Just (Input.placeholder [] (text "Geben Sie eine Adresse ein"))
                  , text = searchstring
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
                List.map informationEntry informations

    in
    column [ height fill, width <| fillPortion 5 ]
        [ header
        , informationPanel
        ]


view model =
    layout [ height fill ] <|
        row [ height fill, width fill ]
            [ topicPanel model.topics model.activeTopic
            , mainPanel model.activeTopic model.informations model.searchString
            ]

