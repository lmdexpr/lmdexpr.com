module Main exposing (..)

import Array exposing (Array)

import Browser
import Task

import Http exposing (Error)

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, i, div, h1, h2 )
import Html.Attributes exposing ( rel, href, class )
import Html.Events exposing ( onClick )

import Bootstrap.Accordion as Accordion
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Card.Block as Block

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
  [ { defaultContent | title = "おすすめラノベ五選", date = "2017-12-19", url = "Posts/2017-12-19-adc.md" }
  , { defaultContent | title = "Chatworkのサマーインターンに参加した話", date = "2019-10-01", url = "Posts/2019-10-01-intern.md" }
  , { defaultContent | title = "『CATS』考察未満感想記事", date = "2020-01-30", url = "Posts/2020-01-30-cats.md" }
  ]

load : Maybe Content -> Cmd Msg
load maybeContent =
  case maybeContent of
    Just content -> Http.get
      { url = "https://raw.githubusercontent.com/lmdexpr/lmdexpr.com/master/" ++ content.url
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
view model = Grid.container []
  [ CDN.stylesheet
  , Html.node "link" [href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"] []
  , Grid.row [ Row.topMd ] [ Grid.col [ Col.middleLg ] [ h1 [] [ text "λx. {0x6c, 0x6d, 0x64, 0x65, 0x78, 0x70, 0x72}" ] ] ]
  , Grid.row [ Row.middleLg ]
    [ Grid.col [ Col.middleLg ]
      [ Accordion.config AccordionMsg
        |> Accordion.withAnimation
        |> Accordion.cards
            ( List.map
              (\content ->
                Accordion.card
                  { id = content.title
                  , options = []
                  , header = Accordion.header []
                    <| Accordion.toggle [] [ text ("[" ++ content.date ++ "] " ++ content.title) ]
                  , blocks = [ Accordion.block [] [ Block.custom <| div [] content.body ] ]
                  }
              ) model.contents
            )
            -- TODO: この実装だと未ロードの記事が表示されない。それもアリだとは思うが……。
        |> Accordion.view model.accordionState
      ]
    ]
  , Grid.row [ Row.bottomXs ] [ Grid.col [ Col.middleLg ] [ text "(c) 2020 ", a [href "http://lmdexpr.com"] [text "Yuki Tajiri(lmdexpr)"] ] ]
  ]

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
