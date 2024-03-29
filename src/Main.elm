module Main exposing (main)

import Array exposing (Array)

import Browser
import Browser.Navigation as Nav

import Url
import Url.Parser as Parser
import Url.Parser.Query as Query

import Json.Decode as D
import Json.Encode exposing (string)

import Http

import Html exposing ( Html, Attribute, span, a, p, text, i, div, h3 )
import Html.Attributes exposing ( href, class, style )
import Html.Events exposing ( onMouseOver )

import Bootstrap.CDN as CDN
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block

import Markdown

import Hex

-- MAIN

main : Program D.Value Model Msg
main = Browser.application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

-- MODEL

type alias Content =
  { title : String
  , date : String
  , url : String
  , body : List (Html Msg)
  }

type alias Model =
  { contents : List Content
  , loadQueue : List JsonContent
  , accordionState : Accordion.State
  , lmdexpr : Array String
  , key : Nav.Key
  , url : Url.Url
  }

-- JSON

type alias JsonContent =
  { title : String
  , date  : String
  , url   : String
  }

decodeOfJsonContent : D.Decoder JsonContent
decodeOfJsonContent =
  D.map3 JsonContent
    (D.field "title" D.string)
    (D.field "date"  D.string)
    (D.field "url"   D.string)

jsonToContent : JsonContent -> Content
jsonToContent json = { title = json.title, date = json.date, url = json.url, body = [] }

-- initialize

aboutme : Content
aboutme =
  { title = "About me"
  , date = String.fromChar '\u{03bb}'
  , url = "about"
  , body =
    [ h3 [] [ text "LMDEXPR a.k.a. Yuki Tajiri" ]
    , p [] [ text "Work at", a [ href "https://aws.amazon.com/jp/" ] [ text "アマゾンウェブサービスジャパン株式会社" ], text " (2023/06/26 ~)" ]
    , p [] [ text "I worked as server-side engineer @", a [ href "https://corp.chatwork.com/ja/" ] [ text "Chatwork Co., Ltd." ], text " (2020/06/01 ~ 2023/06/25)" ]
    , p [] [ text "Dropped out Master of Mathematics @", a [ href "https://www.kobe-u.ac.jp/" ] [ text "Kobe University" ], text " (2019/04 ~ 2020/10)" ]
    , p [] [ text "Graduated Bachelor of Mathematics @", a [ href "https://www.kobe-u.ac.jp/" ] [ text "Kobe University" ], text " (2017/04 ~ 2019/03)" ]
    , p [] [ text "Graduated Associate Degree of Engineering @", a [ href "https://kumamoto-nct.ac.jp/" ] [ text "NIT, Kumamoto College" ], text " (2012/04 ~ 2017/03)" ]
    ]
  }

contact : Content
contact =
  { title = "Contact"
  , date = String.fromChar '\u{03bb}'
  , url = "contact"
  , body =
    [ p [] [ i [ class "fab fa-discord" ] [], text " Discord : lmdexpr" ]
    , p [] [ a [ href "https://bsky.app/profile/lmdexpr.com" ] [ i [ class "fab fa-sun-bright" ] [], text " Bluesky" ] ]
    , p [] [ a [ href "https://twitter.com/lmdexpr" ] [ i [ class "fab fa-twitter" ] [], text " Twitter" ] ]
    , p [] [ a [ href "https://github.com/lmdexpr" ] [ i [ class "fab fa-github" ] [], text " Github" ] ]
    , p [] [ a [ href "https://note.com/lmdexpr" ] [ i [ class "far fa-sticky-note" ] [], text " Note" ] ]
    , p [] [ a [ href "https://zenn.dev/lmdexpr" ] [ i [ class "far fa-pen" ] [], text " Zenn" ] ]
    , p [] [ a [ href "https://www.amazon.jp/hz/wishlist/ls/2Z7ZETUODB09J?ref_=wl_share" ] [ i [ class "fab fa-amazon" ] [], text " Amazon" ] ]
    , p [] [ a [ href "https://lmdexpr.github.io" ] [ i [ class "fas fa-angle-double-left" ] [], text " Ruin" ] ]
    ]
  }

initialModel : Url.Url -> Nav.Key -> List JsonContent -> Model
initialModel url key loadQueue =
  { contents  = [ aboutme, contact ]
  , loadQueue = loadQueue
  , accordionState = Accordion.initialState
  , lmdexpr = Array.fromList [ "0x6c", "0x6d", "0x64", "0x65", "0x78", "0x70", "0x72" ]
  , url = url
  , key = key
  }

init : D.Value -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init v url key =
  loadNext <| initialModel url key <| Result.withDefault [] <| D.decodeValue (D.list decodeOfJsonContent) v

-- ACTION, UPDATE

type Msg
  = Loaded Content (Result Http.Error String)
  | AccordionMsg Accordion.State
  | Convert String Int
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

loadNext : Model -> (Model, Cmd Msg)
loadNext model =
  case model.loadQueue of
    hd :: tl -> ( { model | loadQueue = tl }, load hd )
    _        -> case Parser.parse (Parser.query <| Query.string "content") model.url of
      Just (Just cid) -> ( { model | accordionState = Accordion.initialStateCardOpen cid }, Cmd.none )
      _               -> ( model, Cmd.none)

load : JsonContent -> Cmd Msg
load json =
  Http.get
    { url = "https://raw.githubusercontent.com/lmdexpr/lmdexpr.com/master/Posts/" ++ json.url ++ ".md"
    , expect = Http.expectString <| Loaded <| jsonToContent json
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded content response -> loadNext { model | contents = model.contents ++ [ setContent response content ] }
    AccordionMsg state      -> ( { model | accordionState = state }, Cmd.none )
    Convert s idx           -> ( { model | lmdexpr = Array.set idx (convert s) model.lmdexpr }, Cmd.none )
    LinkClicked request     ->
      case request of
        Browser.Internal url  -> ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href -> ( model, Nav.load href )
    UrlChanged url -> ( { model | url = url }, Cmd.none )

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
    Err err -> { title = "HTTP request ERROR", date = "", url = "", body = [ text <| errorToString err ] }
    Ok  raw -> { content | body = Markdown.toHtml Nothing raw }

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

view : Model -> Browser.Document Msg
view model =
  { title = "lmdexpr.com"
  , body  =
    [ div [ class "responsive" ]
      [ CDN.stylesheet
      , Card.config []
        |> Card.headerH1 [ bg "#222233", tc "#eeeeaa", ft roboto ] (makeTitle model.lmdexpr)
        |> Card.footer [ bg "#222233", tc "#ccccaa", ft noto, style "text-align" "right" ]
          [ text "(c) 2020 "
          , a [href "http://lmdexpr.com"] [text "Yuki Tajiri(lmdexpr)"]
          ]
        |> Card.block [ Block.attrs [ bg "#222233", ft noto] ]
          [ Block.custom <| if List.isEmpty model.loadQueue then viewLoaded model else viewLoading ]
        |> Card.view
      ]
    ]
  }

makeTitle : Array String -> List (Html Msg)
makeTitle lmdexpr =
  (text <| String.fromChar '\u{03bb}' ++ "x. { ")
  :: Array.toList (Array.indexedMap (\ idx s -> span [ onMouseOver (Convert s idx) ] [ text <| s ++ " " ]) lmdexpr)
  ++ [ text "}" ]

viewLoaded : Model -> Html Msg
viewLoaded model = Accordion.config AccordionMsg
  |> Accordion.withAnimation
  |> Accordion.onlyOneOpen
  |> Accordion.cards
    ( List.map
      (\content ->
        Accordion.card
          { id = content.url
          , options = []
          , header = Accordion.header [ bg "#444444" ]
            <| Accordion.toggle [ style "width" "100%", style "text-align" "left" ]
              [ a [ href <| "?content=" ++ content.url, style "padding" "0"]
                [ div [ bg "#444444"
                      , tc "#eeeeee"
                      , style "overflow" "hidden"
                      , style "white-space" "nowrap"
                      , style "text-overflow" "ellipsis" ]
                  [ text ("[" ++ content.date ++ "] " ++ content.title) ]
                ]
              ]
          , blocks = [ Accordion.block [] [ Block.custom <| div [] content.body ] ]
          }
      ) model.contents
    )
  |> Accordion.view model.accordionState

viewLoading : Html Msg
viewLoading =
  div [ style "text-align" "center", ft noto, tc "#ccccaa" ]
    [ i [ class "fa fa-spinner fa-spin" ] [], text " Loading ..." ]


-- Utilities

ft : String -> Attribute Msg
ft = style "font-family"

bg : String -> Attribute Msg
bg = style "background-color"

tc : String -> Attribute Msg
tc = style "color"

roboto : String
roboto = "Roboto Condensed, sans-serif"

noto : String
noto   = "Noto Sans JP, sans-serif"

