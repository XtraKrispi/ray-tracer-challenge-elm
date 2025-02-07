module Lib exposing (..)


epsilon : Float
epsilon =
    0.00001


equal : Float -> Float -> Bool
equal a b =
    abs (a - b) < epsilon
