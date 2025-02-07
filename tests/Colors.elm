module Colors exposing (..)

import Expect
import Lib.Color exposing (add, color, multiply, subtract)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Color operations"
        [ test "Colors are (red, green, blue) tuples"
            (\_ ->
                let
                    c =
                        color -0.5 0.4 1.7
                in
                Expect.all
                    [ \{ red } -> Expect.within (Expect.Absolute 0.00001) red -0.5
                    , \{ green } -> Expect.within (Expect.Absolute 0.00001) green 0.4
                    , \{ blue } -> Expect.within (Expect.Absolute 0.00001) blue 1.7
                    ]
                    c
            )
        , test "Adding colors"
            (\_ ->
                let
                    c1 =
                        color 0.9 0.6 0.75

                    c2 =
                        color 0.7 0.1 0.25

                    result =
                        add c1 c2
                in
                Expect.equal result (color 1.6 0.7 1.0)
            )
        , test "Subtracting colors"
            (\_ ->
                let
                    c1 =
                        color 0.9 0.6 0.75

                    c2 =
                        color 0.7 0.1 0.25

                    result =
                        subtract c1 c2
                in
                Expect.equal (Lib.Color.equal result (color 0.2 0.5 0.5)) True
            )
        , test "Multiplying a color by a scalar"
            (\_ ->
                let
                    c =
                        color 0.2 0.3 0.4

                    result =
                        multiply c 2
                in
                Expect.equal (Lib.Color.equal result (color 0.4 0.6 0.8)) True
            )
        , test "Multiplying colors"
            (\_ ->
                let
                    c1 =
                        color 1 0.2 0.4

                    c2 =
                        color 0.9 1 0.1

                    result =
                        Lib.Color.product c1 c2
                in
                Expect.equal (Lib.Color.equal result (color 0.9 0.2 0.04)) True
            )
        ]
