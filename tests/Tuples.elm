module Tuples exposing (..)

import Expect
import Lib.Tuple exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Arithmetic"
        [ test "Adding two tuples"
            (\_ ->
                let
                    a1 =
                        Tuple 3 -2 5 1

                    a2 =
                        Tuple -2 3 1 0

                    summed =
                        add a1 a2

                    -- tuple(1, 1, 6, 1)
                in
                Expect.equal summed (Tuple 1 1 6 1)
            )
        , test "subtracting two points"
            (\_ ->
                let
                    p1 =
                        point 3 2 1

                    p2 =
                        point 5 6 7

                    result =
                        subtract p1 p2
                in
                Expect.equal result (vector -2 -4 -6)
            )
        , test "subtracting a vector from a point"
            (\_ ->
                let
                    p =
                        point 3 2 1

                    v =
                        vector 5 6 7

                    result =
                        subtract p v
                in
                Expect.equal result (point -2 -4 -6)
            )
        , test "subtracting two vectors"
            (\_ ->
                let
                    v1 =
                        vector 3 2 1

                    v2 =
                        vector 5 6 7

                    result =
                        subtract v1 v2
                in
                Expect.equal result (vector -2 -4 -6)
            )
        , test "subtracting a vector from the zero vector"
            (\_ ->
                let
                    zero =
                        vector 0 0 0

                    v =
                        vector 1 -2 3

                    result =
                        subtract zero v
                in
                Expect.equal result (vector -1 2 -3)
            )
        , test "negating a tuple"
            (\_ ->
                let
                    a =
                        Tuple 1 -2 3 -4

                    result =
                        Lib.Tuple.negate a
                in
                Expect.equal result (Tuple -1 2 -3 4)
            )
        , test "multiplying a tuple by a scalar"
            (\_ ->
                let
                    a =
                        Tuple 1 -2 3 -4

                    result =
                        Lib.Tuple.multiply a 3.5
                in
                Expect.equal result (Tuple 3.5 -7 10.5 -14)
            )
        , test "multiplying a tuple by a fraction"
            (\_ ->
                let
                    a =
                        Tuple 1 -2 3 -4

                    result =
                        Lib.Tuple.multiply a 0.5
                in
                Expect.equal result (Tuple 0.5 -1 1.5 -2)
            )
        , test "dividing a tuple by a scalar"
            (\_ ->
                let
                    a =
                        Tuple 1 -2 3 -4

                    result =
                        Lib.Tuple.divide a 2
                in
                Expect.equal result (Tuple 0.5 -1 1.5 -2)
            )
        , test "Computing the magnitude of vector (1,0,0)"
            (\_ ->
                let
                    a =
                        vector 1 0 0

                    result =
                        Lib.Tuple.magnitude a
                in
                Expect.within (Expect.Absolute 0.0001) result 1
            )
        , test "Computing the magnitude of vector (0,1,0)"
            (\_ ->
                let
                    a =
                        vector 0 1 0

                    result =
                        Lib.Tuple.magnitude a
                in
                Expect.within (Expect.Absolute 0.0001) result 1
            )
        , test "Computing the magnitude of vector (0,0,1)"
            (\_ ->
                let
                    a =
                        vector 0 0 1

                    result =
                        Lib.Tuple.magnitude a
                in
                Expect.within (Expect.Absolute 0.0001) result 1
            )
        , test "Computing the magnitude of vector (1,2,3)"
            (\_ ->
                let
                    a =
                        vector 1 2 3

                    result =
                        Lib.Tuple.magnitude a
                in
                Expect.within (Expect.Absolute 0.0001) result (sqrt 14)
            )
        , test "Computing the magnitude of vector (-1,-2,-3)"
            (\_ ->
                let
                    a =
                        vector -1 -2 -3

                    result =
                        Lib.Tuple.magnitude a
                in
                Expect.within (Expect.Absolute 0.0001) result (sqrt 14)
            )
        , test "Normalizing vector(4,0,0) gives (1,0,0)"
            (\_ ->
                let
                    v =
                        vector 4 0 0

                    result =
                        Lib.Tuple.normalize v
                in
                Expect.equal result (vector 1 0 0)
            )
        , test "Normalizing vector(1,2,3)"
            (\_ ->
                let
                    v =
                        vector 1 2 3

                    result =
                        Lib.Tuple.normalize v
                in
                Expect.equal result (vector (1 / sqrt 14) (2 / sqrt 14) (3 / sqrt 14))
            )
        , test "The magnitude of a normalized vector"
            (\_ ->
                let
                    v =
                        vector 1 2 3

                    result =
                        Lib.Tuple.magnitude (Lib.Tuple.normalize v)
                in
                Expect.within (Expect.Absolute 0.0001) result 1
            )
        , test "The dot product of two tuples"
            (\_ ->
                let
                    a =
                        vector 1 2 3

                    b =
                        vector 2 3 4

                    result =
                        Lib.Tuple.dot a b
                in
                Expect.within (Expect.Absolute 0.0001) result 20
            )
        , test "The cross product of two vectors 1"
            (\_ ->
                let
                    a =
                        vector 1 2 3

                    b =
                        vector 2 3 4

                    result =
                        Lib.Tuple.cross a b
                in
                Expect.equal result (vector -1 2 -1)
            )
        , test "The cross product of two vectors 2"
            (\_ ->
                let
                    a =
                        vector 1 2 3

                    b =
                        vector 2 3 4

                    result =
                        Lib.Tuple.cross b a
                in
                Expect.equal result (vector 1 -2 1)
            )
        , test "Reflecting a vector approaching at 45°"
            (\_ ->
                let
                    v =
                        vector 1 -1 0

                    n =
                        vector 0 1 0

                    r =
                        reflect v n
                in
                Expect.equal r (vector 1 1 0)
            )
        , test "Reflecting a vector off a slanted surface"
            (\_ ->
                let
                    v =
                        vector 0 -1 0

                    n =
                        vector (sqrt 2 / 2) (sqrt 2 / 2) 0

                    r =
                        reflect v n
                in
                assertEqualTuple r (vector 1 0 0)
            )
        ]


assertEqualTuple : Tuple -> Tuple -> Expect.Expectation
assertEqualTuple t1 t2 =
    Expect.equal (Lib.Tuple.equal t1 t2) True
