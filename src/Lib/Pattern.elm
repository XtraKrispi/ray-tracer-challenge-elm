module Lib.Pattern exposing (..)

import Lib.Color exposing (Color, black, color, white)
import Lib.Matrix exposing (FourByFourMatrix, Matrix, identityMatrix)
import Lib.Tuple exposing (Tuple)


type PatternType
    = Stripe
    | Gradient
    | TestPattern
    | Ring
    | Checkers


type alias Pattern =
    { a : Color
    , b : Color
    , transform : Matrix FourByFourMatrix
    , patternType : PatternType
    }


stripePattern : Color -> Color -> Pattern
stripePattern a b =
    { a = a
    , b = b
    , transform = identityMatrix
    , patternType = Stripe
    }


gradientPattern : Color -> Color -> Pattern
gradientPattern a b =
    { a = a
    , b = b
    , transform = identityMatrix
    , patternType = Gradient
    }


ringPattern : Color -> Color -> Pattern
ringPattern a b =
    { a = a
    , b = b
    , transform = identityMatrix
    , patternType = Ring
    }


checkersPattern : Color -> Color -> Pattern
checkersPattern a b =
    { a = a
    , b = b
    , transform = identityMatrix
    , patternType = Checkers
    }


testPattern : Pattern
testPattern =
    { a = white
    , b = white
    , transform = identityMatrix
    , patternType = TestPattern
    }


setTransform : Matrix FourByFourMatrix -> Pattern -> Pattern
setTransform t p =
    { p | transform = t }


patternAt : Tuple -> Pattern -> Color
patternAt point pattern =
    case pattern.patternType of
        Stripe ->
            if modBy 2 (floor point.x) == 0 then
                pattern.a

            else
                pattern.b

        Gradient ->
            let
                distance =
                    Lib.Color.subtract pattern.b pattern.a

                fraction =
                    point.x - toFloat (floor point.x)
            in
            Lib.Color.add pattern.a (Lib.Color.multiply distance fraction)

        TestPattern ->
            color point.x point.y point.z

        Ring ->
            if modBy 2 (floor (sqrt ((point.x * point.x) + (point.z + point.z)))) == 0 then
                pattern.a

            else
                pattern.b

        Checkers ->
            if modBy 2 (floor point.x + floor point.y + floor point.z) == 0 then
                pattern.a

            else
                pattern.b
