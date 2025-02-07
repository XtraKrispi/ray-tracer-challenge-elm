module Transformations exposing (..)

import Expect
import Lib.Matrix exposing (FourColumnRow(..), fourByFour, identityMatrix, invert, multTuple, multiply)
import Lib.Matrix.Transformation exposing (RotationAmount(..), rotationX, rotationY, rotationZ, scaling, shearing, translation, viewTransform)
import Lib.Tuple exposing (Tuple, point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Transformation Operations"
        [ test "Multiplying by a translation matrix"
            (\_ ->
                let
                    transform =
                        translation 5 -3 2

                    p =
                        point -3 4 5
                in
                assertEqualTuple (multTuple transform p) (point 2 1 7)
            )
        , test "Multiplying by the inverse of a translation matrix"
            (\_ ->
                let
                    transform =
                        translation 5 -3 2

                    inv =
                        invert transform

                    p =
                        point -3 4 5
                in
                assertEqualTuple (multTuple inv p) (point -8 7 3)
            )
        , test "Translation does not affect vectors"
            (\_ ->
                let
                    transform =
                        translation 5 -3 2

                    v =
                        vector -3 4 5
                in
                assertEqualTuple (multTuple transform v) v
            )
        , test "A scaling matrix applied to a point"
            (\_ ->
                let
                    transform =
                        scaling 2 3 4

                    p =
                        point -4 6 8
                in
                assertEqualTuple (multTuple transform p) (point -8 18 32)
            )
        , test "A scaling matrix applied to a vector"
            (\_ ->
                let
                    transform =
                        scaling 2 3 4

                    v =
                        vector -4 6 8
                in
                assertEqualTuple (multTuple transform v) (vector -8 18 32)
            )
        , test "Multiplying by the inverse of a scaling matrix"
            (\_ ->
                let
                    transform =
                        scaling 2 3 4

                    inv =
                        invert transform

                    v =
                        vector -4 6 8
                in
                assertEqualTuple (multTuple inv v) (vector -2 2 2)
            )
        , test "Rotating a point around the x axis"
            (\_ ->
                let
                    p =
                        point 0 1 0

                    halfQuarter =
                        rotationX (Radians (pi / 4))

                    fullQuarter =
                        rotationX (Radians (pi / 2))
                in
                Expect.all
                    [ \_ -> assertEqualTuple (multTuple halfQuarter p) (point 0 (sqrt 2 / 2) (sqrt 2 / 2))
                    , \_ -> assertEqualTuple (multTuple fullQuarter p) (point 0 0 1)
                    ]
                    ()
            )
        , test "The inverse of an x-rotation rotates in the opposite direction"
            (\_ ->
                let
                    p =
                        point 0 1 0

                    halfQuarter =
                        rotationX (Radians (pi / 4))

                    inv =
                        invert halfQuarter
                in
                assertEqualTuple (multTuple inv p) (point 0 (sqrt 2 / 2) (negate (sqrt 2 / 2)))
            )
        , test "Rotating a point around the y axis"
            (\_ ->
                let
                    p =
                        point 0 0 1

                    halfQuarter =
                        rotationY (Radians (pi / 4))

                    fullQuarter =
                        rotationY (Radians (pi / 2))
                in
                Expect.all
                    [ \_ -> assertEqualTuple (multTuple halfQuarter p) (point (sqrt 2 / 2) 0 (sqrt 2 / 2))
                    , \_ -> assertEqualTuple (multTuple fullQuarter p) (point 1 0 0)
                    ]
                    ()
            )
        , test "Rotating a point around the z axis"
            (\_ ->
                let
                    p =
                        point 0 1 0

                    halfQuarter =
                        rotationZ (Radians (pi / 4))

                    fullQuarter =
                        rotationZ (Radians (pi / 2))
                in
                Expect.all
                    [ \_ -> assertEqualTuple (multTuple halfQuarter p) (point (negate (sqrt 2 / 2)) (sqrt 2 / 2) 0)
                    , \_ -> assertEqualTuple (multTuple fullQuarter p) (point -1 0 0)
                    ]
                    ()
            )
        , test "A shearing transformation moves x in proportion to y"
            (\_ ->
                let
                    transform =
                        shearing 1 0 0 0 0 0

                    p =
                        point 2 3 4
                in
                assertEqualTuple (multTuple transform p) (point 5 3 4)
            )
        , test "A shearing transformation moves x in proportion to z"
            (\_ ->
                let
                    transform =
                        shearing 0 1 0 0 0 0

                    p =
                        point 2 3 4
                in
                assertEqualTuple (multTuple transform p) (point 6 3 4)
            )
        , test "A shearing transformation moves y in proportion to x"
            (\_ ->
                let
                    transform =
                        shearing 0 0 1 0 0 0

                    p =
                        point 2 3 4
                in
                assertEqualTuple (multTuple transform p) (point 2 5 4)
            )
        , test "A shearing transformation moves y in proportion to z"
            (\_ ->
                let
                    transform =
                        shearing 0 0 0 1 0 0

                    p =
                        point 2 3 4
                in
                assertEqualTuple (multTuple transform p) (point 2 7 4)
            )
        , test "A shearing transformation moves z in proportion to x"
            (\_ ->
                let
                    transform =
                        shearing 0 0 0 0 1 0

                    p =
                        point 2 3 4
                in
                assertEqualTuple (multTuple transform p) (point 2 3 6)
            )
        , test "A shearing transformation moves z in proportion to y"
            (\_ ->
                let
                    transform =
                        shearing 0 0 0 0 0 1

                    p =
                        point 2 3 4
                in
                assertEqualTuple (multTuple transform p) (point 2 3 7)
            )
        , test "Individual transformations are applied in sequence"
            (\_ ->
                let
                    p =
                        point 1 0 1

                    a =
                        rotationX (Radians (pi / 2))

                    b =
                        scaling 5 5 5

                    c =
                        translation 10 5 7

                    p2 =
                        multTuple a p

                    p3 =
                        multTuple b p2

                    p4 =
                        multTuple c p3
                in
                Expect.all
                    [ \_ -> assertEqualTuple p2 (point 1 -1 0)
                    , \_ -> assertEqualTuple p3 (point 5 -5 0)
                    , \_ -> assertEqualTuple p4 (point 15 0 7)
                    ]
                    ()
            )
        , test "Chained transformations must be applied in reverse order"
            (\_ ->
                let
                    p =
                        point 1 0 1

                    a =
                        rotationX (Radians (pi / 2))

                    b =
                        scaling 5 5 5

                    c =
                        translation 10 5 7

                    t =
                        multiply c (multiply b a)
                in
                assertEqualTuple (multTuple t p) (point 15 0 7)
            )
        , test "The transformation matrix for the default orientation"
            (\_ ->
                let
                    from =
                        point 0 0 0

                    to =
                        point 0 0 -1

                    up =
                        vector 0 1 0

                    t =
                        viewTransform from to up
                in
                Expect.equal t identityMatrix
            )
        , test "A view transformation matrix looking in positive z direction"
            (\_ ->
                let
                    from =
                        point 0 0 0

                    to =
                        point 0 0 1

                    up =
                        vector 0 1 0

                    t =
                        viewTransform from to up
                in
                Expect.equal t (scaling -1 1 -1)
            )
        , test "The view transformation moves the world"
            (\_ ->
                let
                    from =
                        point 0 0 8

                    to =
                        point 0 0 0

                    up =
                        vector 0 1 0

                    t =
                        viewTransform from to up
                in
                Expect.equal t (translation 0 0 -8)
            )
        , test "An arbitrary view transformation"
            (\_ ->
                let
                    from =
                        point 1 3 2

                    to =
                        point 4 -2 8

                    up =
                        vector 1 1 0

                    t =
                        viewTransform from to up

                    expected =
                        fourByFour
                            (FourColumnRow -0.50709 0.50709 0.67612 -2.36643)
                            (FourColumnRow 0.76772 0.60609 0.12122 -2.82843)
                            (FourColumnRow -0.35857 0.59761 -0.71714 0.0)
                            (FourColumnRow 0.0 0.0 0.0 1.0)
                in
                Expect.equal (Lib.Matrix.equal t expected) True
            )
        ]


assertEqualTuple : Tuple -> Tuple -> Expect.Expectation
assertEqualTuple t1 t2 =
    Expect.equal (Lib.Tuple.equal t1 t2) True
