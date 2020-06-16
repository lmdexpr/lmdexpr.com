module Main exposing (..)

import Array exposing (Array)

import Browser
import Task

import Http exposing (Error)

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, i, div, h1, h2 )
import Html.Attributes exposing ( rel, href, class, style, width )
import Html.Events exposing ( onClick )

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Text as Text
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Spacing exposing (mb0)

import Markdown

-- MAIN

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

-- MODEL

type alias Content =
  { title : String
  , date : String
  , url : String
  , body : List (Html Msg)
  , loaded : Bool
  }

type alias Model =
  { contents : List Content
  , loadQueue : Maybe (List Content)
  , accordionState : Accordion.State
  }

aboutme : Content
aboutme =
  { title = "About me"
  , date = "λ"
  , url = "dammy"
  , body =
    [ text "TODO"
    ]
  , loaded = True
  }

initialModel : Model
initialModel =
  { contents  = [ aboutme ]
  , loadQueue = Nothing
  , accordionState = Accordion.initialState
  }

defaultContent : Content
defaultContent = { title = "", date = "", url = "", body = [], loaded = False }

initialLoadQueue : List Content
initialLoadQueue =
  [ { defaultContent | title = "おすすめラノベ五選", date = "2017-12-19", url = "2017-12-19-adc.md" }
  , { defaultContent | title = "Chatworkのサマーインターンに参加した話", date = "2019-10-01", url = "2019-10-01-intern.md" }
  , { defaultContent | title = "『CATS』考察未満感想記事", date = "2020-01-30", url = "2020-01-30-cats.md" }
  ]

load : Maybe Content -> Cmd Msg
load maybeContent =
  case maybeContent of
    Just content -> Http.get
      { url = "https://raw.githubusercontent.com/lmdexpr/lmdexpr.com/master/Posts/" ++ content.url
      , expect = Http.expectString <| Loaded content
      }
    Nothing -> Cmd.none

init : () -> (Model, Cmd Msg)
init _ = ( { initialModel | loadQueue = List.tail initialLoadQueue } , load <| List.head initialLoadQueue )


-- ACTION, UPDATE

type Msg
  = Loaded Content (Result Http.Error String)
  | AccordionMsg Accordion.State

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded content response ->
      ({ model
       | contents = model.contents ++ [ setContent response content ]
       , loadQueue = Maybe.andThen List.tail model.loadQueue
       }
      , load <| Maybe.andThen List.head model.loadQueue)
    AccordionMsg state -> ( { model | accordionState = state }, Cmd.none )

setContent : Result Http.Error String -> Content -> Content
setContent response content =
  case response of
    Err err -> { title = "HTTP request ERROR", date = "", url = "", body = [ text <| errorToString err ], loaded = True }
    Ok  raw -> { content | body = Markdown.toHtml Nothing raw, loaded = True }

errorToString : Http.Error -> String
errorToString error =
  case error of
    Http.BadUrl url -> "The URL " ++ url ++ " was invalid"
    Http.Timeout -> "Unable to reach the server, try again"
    Http.NetworkError -> "Unable to reach the server, check your network connection"
    Http.BadStatus 500 -> "The server had a problem, try again later"
    Http.BadStatus 400 -> "Verify your information and try again"
    Http.BadStatus _ -> "Unknown error"
    Http.BadBody errorMessage -> errorMessage


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Accordion.subscriptions model.accordionState AccordionMsg

-- VIEW

view : Model -> Html Msg
view model = div [ class "responsive", style "margin" "auto", style "padding" "60px", style "width" "80%", style "opacity" "0.9" ]
  [ CDN.stylesheet
  , Html.node "link" [href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"] []
  , Html.node "link" [href "https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@300&family=Roboto+Condensed:wght@700&display=swap", rel "stylesheet"] []
  , Card.config configs
    |> Card.headerH1 [ background "#222233", textcolor "#eeeeaa", font "Roboto Condensed, sans-serif" ]
      [ text "λx. {0x6c, 0x6d, 0x64, 0x65, 0x78, 0x70, 0x72}" ]
    |> Card.footer [ background "#222233", textcolor "#ccccaa", font "Noto Sans JP, sans-serif", style "text-align" "right" ]
      [ text "(c) 2020 "
      , a [href "http://lmdexpr.com"] [text "Yuki Tajiri(lmdexpr)"]
      ]
    |> Card.block [ Block.attrs [ background "#222233", font "Noto Sans JP, sans-serif" ] ]
      [ Block.custom <| case model.loadQueue of
        Nothing -> viewLoaded model
        _       -> viewLoading
      ]
    |> Card.view
  ]

configs : List (Card.Option Msg)
configs = [ Card.attrs [ style "min-width" "800px" ] ]

viewMain : Model -> Html Msg
viewMain model =
  case model.loadQueue of
    Nothing -> viewLoaded model
    _       -> viewLoading

viewLoaded : Model -> Html Msg
viewLoaded model = Accordion.config AccordionMsg
  |> Accordion.withAnimation
  |> Accordion.cards
    ( List.map
      (\content ->
        Accordion.card
          { id = content.title
          , options = []
          , header = Accordion.header [ style "background-color" "#444444" ]
            <| Accordion.toggle [] [ Button.button [ Button.attrs [ style "background-color" "#444444", style "color" "#eeeeee" ] ]
              [ text ("[" ++ content.date ++ "] " ++ content.title) ] ]
          , blocks = [ Accordion.block [] [ Block.custom <| div [] content.body ] ]
          }
      ) model.contents
    )
  |> Accordion.view model.accordionState

viewLoading : Html Msg
viewLoading =
  div [ style "text-align" "center", font "Noto Sans JP, sans-serif", textcolor "#ccccaa" ] [ text "Loading ..." ]

font = style "font-family"
background = style "background-color"
textcolor = style "color"
