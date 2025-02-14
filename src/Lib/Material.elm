module Lib.Material exposing (..)

import Lib.Color exposing (Color, black, white)
import Lib.Pattern exposing (Pattern)


type alias Material =
    { color : Color
    , ambient : Float
    , diffuse : Float
    , specular : Float
    , shininess : Float
    , pattern : Maybe Pattern
    , reflective : Float
    , transparency : Float
    , refractiveIndex : Float
    }


material : Material
material =
    { color = white
    , ambient = 0.1
    , diffuse = 0.9
    , specular = 0.9
    , shininess = 200
    , reflective = 0
    , pattern = Nothing
    , transparency = 0
    , refractiveIndex = 1
    }
