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

type alias BlogContent =
  { title : String
  , date : String
  , url : String
  , content : List (Html Msg)
  }

type alias Model =
  { contents   : Array BlogContent
  , loading    : Maybe Int
  , accordionState : Accordion.State
  }

blogContents : Array BlogContent
blogContents = Array.fromList
  [ {title = "About me", date = "none", url = "dammy", content = aboutme}
  , {title = "おすすめラノベ五選", date = "2017-12-19", url = "Posts/2017-12-19-adc.md", content = []}
  , {title = "Chatworkのサマーインターンに参加した話", date = "2019-10-01", url = "Posts/2019-10-01-intern.md", content = []}
  , {title = "『CATS』考察未満感想記事", date = "2020-01-30", url = "Posts/2020-01-30-cats.md", content = []}
  ]

init : () -> (Model, Cmd Msg)
init _ = ( { contents = blogContents, loading = Nothing, accordionState = Accordion.initialState } , Cmd.none )


-- ACTION, UPDATE

type Msg
  = RequestContent Int String
  | Loaded (Result Http.Error String)
  | AccordionMsg Accordion.State

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestContent idx url -> ({ model | loading = if idx /= 0 then Just idx else Nothing }, getContent url)
    Loaded response ->
      case model.loading of
        Nothing  -> (model, Cmd.none)
        Just idx ->
          ({ model
           | loading = Nothing
           , contents = Array.set idx (loadContent response <| Array.get idx model.contents) model.contents
           }
          , Cmd.none)
    AccordionMsg state -> ( { model | accordionState = state }, Cmd.none )

getContent : String -> Cmd Msg
getContent url =
  Http.get
    { url = "https://raw.githubusercontent.com/lmdexpr/lmdexpr.com/master/" ++ url
    , expect = Http.expectString Loaded
    }

loadContent : Result Http.Error String -> Maybe BlogContent -> BlogContent
loadContent response maybeContent =
  case maybeContent of
    Just blogContent ->
      case response of
        Err err -> { title = "HTTP request ERROR", date = "", url = "", content = [ text <| errorToString err ] }
        Ok  raw -> { blogContent | content = Markdown.toHtml Nothing raw }
    Nothing -> { title = "Index out of range", date = "", url = "", content = [] }

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
          ( Array.toList
            ( Array.indexedMap
              (\idx content ->
                Accordion.card
                  { id = String.fromInt idx
                  , options = []
                  , header = Accordion.header [ onClick <| RequestContent idx content.url ] <| Accordion.toggle [] [ span [ class "fa fa-car" ] [], text content.title ]
                  , blocks = [ Accordion.block [] [ Block.custom <| div [] <| Maybe.withDefault [ text "loading..." ] <| Maybe.map .content <| Array.get idx model.contents ] ]
                  }
              ) model.contents
            )
          )
        |> Accordion.view model.accordionState
      ]
    ]
  , Grid.row [ Row.bottomXs ] [ Grid.col [ Col.middleLg ] [ text "(c) 2020 ", a [href "http://lmdexpr.com"] [text "Yuki Tajiri(lmdexpr)"] ] ]
  ]

aboutme : List (Html Msg)
aboutme =
  [ -- TODO: write down about me
  ]
