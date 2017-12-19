module Page exposing (..)

import Time exposing (Time, millisecond)

import Material
import Material.Footer
import Material.Typography
import Material.Grid    exposing (grid, cell, size, Device(..))
import Material.Options exposing (div, css, stylesheet, Property)

-- MODEL

type alias FlakeParams =
  { styles : List (Property c m)
  , dy     : Float
  , delay  : Int
  }

flake_params : List FlakeParams
flake_params = 
  let flake n pos =
        [ css "position"      "absolute"
        , css "display"       "block"
        , css "border-radius" "2.5px"
        , css "background"    "#FFF"
        , css "left"          pos
        , css "width"         <| toString w
        , css "height"        <| toString h
        ]
  in [ {flake 5 "20px",  15, 0}
     , {flake 2 "40px",  11, 1}
     , {flake 5 "60px",  12, 2}
     , {flake 2 "80px",  13, 0}
     , {flake 5 "100px", 13, 3}
     , {flake 2 "150px", 14, 4}
     , {flake 5 "170px", 11, 4}
     , {flake 2 "200px", 10, 5}
     ]

type Language = ENG  | ESP
type Page     = Home | Profile | Blog | Post Int | Contact

type alias Model =
  { flake_top   : List Int
  , flake_delay : List Int
  , angle_snow  : Int
  , language    : Language
  , page        : Page
  , mdl         : Material.Model
  }

model_eng : Model
model_eng =
  { flake_top   = repeat << length flake_params <| 0
  , flake_delay = map .delay flake_params
  , angle_snow  = 0
  , language    = ENG
  , page        = Home
  , mdl         = Material.model
  }

model_esp : Model
model_esp = { model_eng | language = ESP }

-- ACTION, UPDATE

type Msg = Tick | Mdl (Material.Msg Msg)

subscriptions : Model -> Sub Msg
subscriptions model = Time.every (300 * millisecond) Tick

animate : Model -> Model
animate model =
  { model
  | flake_delay = map (\fd -> if fd > 0 then fd - 1 else 0) model.flake_delay
  , flake_top   = map3 (\ft fd dy -> if fd > 0 then ft else ft + dy) model.flake_top model.flake_delay <| map .dy flake_params
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Mdl msg' -> Material.update msg' model
    Tick     -> (animate model, Cmd.none)

-- VIEW

name_txt : Language -> String
name_txt lng =
  case lng of
    ENG -> "yuki"
    ESP -> "negxo"

profile_txt : Language -> String
profile_txt lng = 
  case lng of
    ENG -> "profile"
    ESP -> "profilo"

blog_txt : Language -> String
blog_txt lng = 
  case lng of
    ENG -> "blog"
    ESP -> "blogo"

contact_txt : Language -> String
contact_txt lng = 
  case lng of
    ENG -> "contact"
    ESP -> "kontakto"

bg_color : Language -> Style
bg_color lng = css "background-color" <| 
  case lng of
    ENG -> "#BECCFF"
    ESP -> "#FFB6A0"

name_color : Language -> Style
name_color lng = css "background-color" <| 
  case lng of
    ENG -> "#142744"
    ESP -> "#FF5930"

view : Html a -> Html a -> Model -> Html a
view tab contents model =
  let title_style =
        [ Typography.center
        , css "top" "70px"
        , css "position" "fixed"
        ]
  in
      grid [ noSpanning, bg_color model.language ]
        [ stylesheet "@import url('https://fonts.googleapis.com/css?family=Raleway');"
        , cell [ title_style, name_color model.language, size All 2, size All 4 ]
            [ div [] <| (map2 (++) (map (css "top" << toString) model.flake_top) <| toList <| map .styles flakes) ++ [h1 [] [name_txt model.language]]
            ]
        , tab
        , contents
        , Footer.mini []
          { left  = Footer.left [] []
          , right = Footer.right [] [ Footer.logo [] [ Footer.html <| text "LMDEXPR.COM" ] ]
          }
        ]

