module Main exposing (..)

import Array exposing (Array)

import Browser
import Task

import Http exposing (Error)

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, i, div, h1, h3 )
import Html.Attributes exposing ( rel, href, class, style, width )
import Html.Events exposing ( onClick, onMouseOver )

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

import Hex

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
  , lmdexpr : Array String
  }

aboutme : Content
aboutme =
  { title = "About me"
  , date = "λ"
  , url = "dammy"
  , body =
    [ h3 [] [ text "LMDEXPR a.k.a. Yuki Tajiri" ]
    , p [] [ text "I work as server-side engineer @", a [ href "https://corp.chatwork.com/ja/" ] [ text "Chatwork Inc." ], text " (2020/06/01 ~)" ]
    , p [] [ text "Droped out Master of Mathematics @", a [ href "https://www.kobe-u.ac.jp/" ] [ text "Kobe University" ], text " (2019/04 ~ 2020/10)" ]
    , p [] [ text "Graduated Bachelor of Mathematics @", a [ href "https://www.kobe-u.ac.jp/" ] [ text "Kobe University" ], text " (2017/04 ~ 2019/03)" ]
    , p [] [ text "Graduated Associate Degree of Engineering @", a [ href "https://kumamoto-nct.ac.jp/" ] [ text "NIT, Kumamoto College" ], text " (2017/04 ~ 2019/03)" ]
    ]
  , loaded = True
  }

contact : Content
contact =
  { title = "Contact"
  , date = "λ"
  , url = "dammy"
  , body =
    [ p [] [ i [ class "far fa-envelope" ] [], text " tajiri@chatwork.com" ]
    , p [] [ a [ href "https://twitter.com/lmdexpr" ] [ i [ class "fab fa-twitter" ] [], text " Twitter" ] ]
    , p [] [ a [ href "https://github.com/lmdexpr" ] [ i [ class "fab fa-github" ] [], text " Github" ] ]
    , p [] [ a [ href "https://note.com/lmdexpr" ] [ i [ class "far fa-sticky-note" ] [], text " Note" ] ]
    , p [] [ a [ href "https://www.amazon.jp/hz/wishlist/ls/2Z7ZETUODB09J?ref_=wl_share" ] [ i [ class "fab fa-amazon" ] [], text " Amazon" ] ]
    , p [] [ a [ href "https://lmdexpr.github.io" ] [ i [ class "fas fa-angle-double-left" ] [], text " Ruin" ] ]
    ]
  , loaded = True
  }

initialModel : Model
initialModel =
  { contents  = [ aboutme, contact ]
  , loadQueue = Nothing
  , accordionState = Accordion.initialState
  , lmdexpr = Array.fromList [ "0x6c", "0x6d", "0x64", "0x65", "0x78", "0x70", "0x72" ]
  }

defaultContent : Content
defaultContent = { title = "", date = "", url = "", body = [], loaded = False }

initialLoadQueue : List Content
initialLoadQueue =
  [ { defaultContent | title = "おすすめラノベ五選", date = "2017-12-19", url = "2017-12-19-adc.md" }
  , { defaultContent | title = "Chatworkサマーインターン", date = "2019-10-01", url = "2019-10-01-intern.md" }
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
  | Convert String Int

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
    Convert s idx -> ( { model | lmdexpr = Array.set idx (convert s) model.lmdexpr }, Cmd.none )

convert : String -> String
convert target =
  if String.startsWith "0x" target then
    case String.slice 2 4 target |> Hex.fromString of
      Ok n -> Char.fromCode n |> String.fromChar
      _    -> "ERROR : hex to char"
  else
    case String.uncons target of
      Just (c, _) -> Char.toCode c |> Hex.toString |> (\s -> "0x" ++ s)
      _           -> "ERROR : char to hex"

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
view model = div [ class "responsive" ]
  [ CDN.stylesheet
  , Html.node "link" [href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"] []
  , Html.node "link" [href "https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@300&family=Roboto+Condensed:wght@700&display=swap", rel "stylesheet"] []
  , Card.config []
    |> Card.headerH1 [ background "#222233", textcolor "#eeeeaa", font "Roboto Condensed, sans-serif" ] (makeTitle model.lmdexpr)
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

makeTitle : Array String -> List (Html Msg)
makeTitle lmdexpr =
  [ text "λx. { " ]
  ++ Array.toList (Array.indexedMap (\ idx s -> span [ onMouseOver (Convert s idx) ] [ text <| s ++ " " ]) lmdexpr)
  ++ [ text "}" ]

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
            <| Accordion.toggle [ style "width" "100%", style "text-align" "left", style "padding" "0" ]
              [ div [ style "background-color" "#444444"
                    , style "color" "#eeeeee"
                    , style "overflow" "hidden"
                    , style "white-space" "nowrap"
                    , style "width" "auto"
                    , style "text-overflow" "ellipsis" ]
                [ text ("[" ++ content.date ++ "] " ++ content.title) ]
              ]
          , blocks = [ Accordion.block [] [ Block.custom <| div [] content.body ] ]
          }
      ) model.contents
    )
  |> Accordion.view model.accordionState

viewLoading : Html Msg
viewLoading =
  div [ style "text-align" "center", font "Noto Sans JP, sans-serif", textcolor "#ccccaa" ]
    [ i [ class "fa fa-spinner fa-spin" ] [], text " Loading ..." ]

font = style "font-family"
background = style "background-color"
textcolor = style "color"
