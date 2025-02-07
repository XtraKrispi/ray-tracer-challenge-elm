module Lib.Matrix.Transformation exposing (..)

import Lib.Matrix exposing (FourByFourMatrix, FourColumnRow(..), Matrix, fourByFour, multiply)
import Lib.Tuple exposing (Tuple, cross, normalize, subtract)


translation : Float -> Float -> Float -> Matrix FourByFourMatrix
translation x y z =
    fourByFour
        (FourColumnRow 1 0 0 x)
        (FourColumnRow 0 1 0 y)
        (FourColumnRow 0 0 1 z)
        (FourColumnRow 0 0 0 1)


scaling : Float -> Float -> Float -> Matrix FourByFourMatrix
scaling x y z =
    fourByFour
        (FourColumnRow x 0 0 0)
        (FourColumnRow 0 y 0 0)
        (FourColumnRow 0 0 z 0)
        (FourColumnRow 0 0 0 1)


type RotationAmount
    = Radians Float
    | Degrees Float


toRadians : RotationAmount -> Float
toRadians amt =
    case amt of
        Radians rad ->
            rad

        Degrees deg ->
            (deg / 180) * pi


rotationX : RotationAmount -> Matrix FourByFourMatrix
rotationX amt =
    let
        r =
            toRadians amt
    in
    fourByFour
        (FourColumnRow 1 0 0 0)
        (FourColumnRow 0 (cos r) (negate (sin r)) 0)
        (FourColumnRow 0 (sin r) (cos r) 0)
        (FourColumnRow 0 0 0 1)


rotationY : RotationAmount -> Matrix FourByFourMatrix
rotationY amt =
    let
        r =
            toRadians amt
    in
    fourByFour
        (FourColumnRow (cos r) 0 (sin r) 0)
        (FourColumnRow 0 1 0 0)
        (FourColumnRow (negate (sin r)) 0 (cos r) 0)
        (FourColumnRow 0 0 0 1)


rotationZ : RotationAmount -> Matrix FourByFourMatrix
rotationZ amt =
    let
        r =
            toRadians amt
    in
    fourByFour
        (FourColumnRow (cos r) (negate (sin r)) 0 0)
        (FourColumnRow (sin r) (cos r) 0 0)
        (FourColumnRow 0 0 1 0)
        (FourColumnRow 0 0 0 1)


shearing : Float -> Float -> Float -> Float -> Float -> Float -> Matrix FourByFourMatrix
shearing xy xz yx yz zx zy =
    fourByFour
        (FourColumnRow 1 xy xz 0)
        (FourColumnRow yx 1 yz 0)
        (FourColumnRow zx zy 1 0)
        (FourColumnRow 0 0 0 1)


viewTransform : Tuple -> Tuple -> Tuple -> Matrix FourByFourMatrix
viewTransform from to up =
    let
        fromN =
            Lib.Tuple.negate from

        forward =
            normalize (subtract to from)

        forwardN =
            Lib.Tuple.negate forward

        upn =
            normalize up

        left =
            cross forward upn

        trueUp =
            cross left forward

        orientation =
            fourByFour
                (FourColumnRow left.x left.y left.z 0)
                (FourColumnRow trueUp.x trueUp.y trueUp.z 0)
                (FourColumnRow forwardN.x forwardN.y forwardN.z 0)
                (FourColumnRow 0 0 0 1)
    in
    multiply orientation (translation fromN.x fromN.y fromN.z)



{-
   function view_transform(from, to, up)
   forward ← normalize(to - from)
   upn ← normalize(up)
   left ← cross(forward, upn)
   true_up ← cross(left, forward)
   orientation ← matrix( left.x, left.y, left.z, 0,
   true_up.x, true_up.y, true_up.z, 0,
   -forward.x, -forward.y, -forward.z, 0,
   0, 0, 0, 1)
   return orientation * translation(-from.x, -from.y, -from.z)
   end function

-}
