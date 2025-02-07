module Lib.Light exposing (..)

import Lib.Color exposing (Color)
import Lib.Tuple exposing (Tuple)


type alias Light =
    { position : Tuple, intensity : Color }


pointLight : Tuple -> Color -> Light
pointLight =
    Light
