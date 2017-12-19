module Main exposing (..)

import Time exposing (Time, millisecond)

import Material
import Material.Footer
import Material.Typography
import Material.Grid    exposing (grid, cell, size, Device(..))
import Material.Options exposing (div, css, stylesheet, Property)

import Page

main = program
  { init   = (Page.model_eng, Cmd.none)
  , update = Page.update
  , subscriptions = Page.subscriptions
  , view   = Page.view tab contents
  }

tab : Html a
tab = cell [] []

contents : Html a
contents = -- todo
