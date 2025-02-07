module Lib.Color exposing (..)

import Lib


type alias Color =
    { red : Float, green : Float, blue : Float }


equal : Color -> Color -> Bool
equal c1 c2 =
    Lib.equal c1.red c2.red && Lib.equal c1.green c2.green && Lib.equal c1.blue c2.blue


color : Float -> Float -> Float -> Color
color =
    Color


add : Color -> Color -> Color
add c1 c2 =
    Color (c1.red + c2.red) (c1.green + c2.green) (c1.blue + c2.blue)


subtract : Color -> Color -> Color
subtract c1 c2 =
    Color (c1.red - c2.red) (c1.green - c2.green) (c1.blue - c2.blue)


multiply : Color -> Float -> Color
multiply c s =
    Color (c.red * s) (c.green * s) (c.blue * s)


product : Color -> Color -> Color
product c1 c2 =
    Color (c1.red * c2.red) (c1.green * c2.green) (c1.blue * c2.blue)


white : Color
white =
    color 1 1 1


black : Color
black =
    color 0 0 0
