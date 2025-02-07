module Lib.Ray exposing (..)

import Lib.Matrix exposing (FourByFourMatrix, Matrix, multTuple)
import Lib.Tuple exposing (Tuple, add, multiply)


type alias Ray =
    { origin : Tuple
    , direction : Tuple
    }


position : Ray -> Float -> Tuple
position { origin, direction } t =
    add origin (multiply direction t)


transform : Ray -> Matrix FourByFourMatrix -> Ray
transform { origin, direction } m =
    Ray (multTuple m origin) (multTuple m direction)
