module Patterns exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Color exposing (Color, black, color, white)
import Lib.Matrix.Transformation exposing (scaling, translation)
import Lib.Object exposing (Id(..), patternAtShape, setTransform, sphere)
import Lib.Pattern exposing (checkersPattern, gradientPattern, patternAt, ringPattern, stripePattern, testPattern)
import Lib.Tuple exposing (point)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Patterns Tests"
        [ test "A pattern with an object transformation"
            (\_ ->
                let
                    shape =
                        sphere (Id 1)
                            |> Lib.Object.setTransform (scaling 2 2 2)

                    pattern =
                        testPattern

                    c =
                        patternAtShape (point 2 3 4) pattern shape
                in
                assertColorEqual c (color 1 1.5 2)
            )
        , test "A pattern with a pattern transformation"
            (\_ ->
                let
                    shape =
                        sphere (Id 1)

                    pattern =
                        testPattern
                            |> Lib.Pattern.setTransform (scaling 2 2 2)

                    c =
                        patternAtShape (point 2 3 4) pattern shape
                in
                assertColorEqual c (color 1 1.5 2)
            )
        , test "A pattern with both an object and a pattern transformation"
            (\_ ->
                let
                    shape =
                        sphere (Id 1)
                            |> Lib.Object.setTransform (scaling 2 2 2)

                    pattern =
                        testPattern
                            |> Lib.Pattern.setTransform (translation 0.5 1 1.5)

                    c =
                        patternAtShape (point 2.5 3 3.5) pattern shape
                in
                assertColorEqual c (color 0.75 0.5 0.25)
            )
        , test "Creating a stripe pattern"
            (\_ ->
                let
                    pattern =
                        stripePattern white black
                in
                Expect.all
                    [ \_ -> Expect.equal pattern.a white
                    , \_ -> Expect.equal pattern.b black
                    ]
                    ()
            )
        , test "A stripe pattern is constant in y"
            (\_ ->
                let
                    pattern =
                        stripePattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 1 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 2 0) pattern) white
                    ]
                    ()
            )
        , test "A stripe pattern is constant in z"
            (\_ ->
                let
                    pattern =
                        stripePattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 0 1) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 0 2) pattern) white
                    ]
                    ()
            )
        , test "A stripe pattern alternates in x"
            (\_ ->
                let
                    pattern =
                        stripePattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0.9 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 1 0 0) pattern) black
                    , \_ -> assertColorEqual (patternAt (point -0.1 0 0) pattern) black
                    , \_ -> assertColorEqual (patternAt (point -1 0 0) pattern) black
                    , \_ -> assertColorEqual (patternAt (point -1.1 0 0) pattern) white
                    ]
                    ()
            )
        , test "Stripes with an object transformation"
            (\_ ->
                let
                    object =
                        sphere (Id 1)
                            |> setTransform (scaling 2 2 2)

                    pattern =
                        stripePattern white black

                    c =
                        patternAtShape (point 1.5 0 0) pattern object
                in
                assertColorEqual c white
            )
        , test "Stripes with a pattern transformation"
            (\_ ->
                let
                    object =
                        sphere (Id 1)

                    pattern =
                        stripePattern white black
                            |> Lib.Pattern.setTransform (scaling 2 2 2)

                    c =
                        patternAtShape (point 1.5 0 0) pattern object
                in
                assertColorEqual c white
            )
        , test "Stripes with both an object and a pattern transformation"
            (\_ ->
                let
                    object =
                        sphere (Id 1)
                            |> setTransform (scaling 2 2 2)

                    pattern =
                        stripePattern white black
                            |> Lib.Pattern.setTransform (translation 0.5 0 0)

                    c =
                        patternAtShape (point 2.5 0 0) pattern object
                in
                assertColorEqual c white
            )
        , test "A gradient linearly interpolates between colors"
            (\_ ->
                let
                    pattern =
                        gradientPattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0.25 0 0) pattern) (color 0.75 0.75 0.75)
                    , \_ -> assertColorEqual (patternAt (point 0.5 0 0) pattern) (color 0.5 0.5 0.5)
                    , \_ -> assertColorEqual (patternAt (point 0.75 0 0) pattern) (color 0.25 0.25 0.25)
                    ]
                    ()
            )
        , test "A ring should extend in both x and z"
            (\_ ->
                let
                    pattern =
                        ringPattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 1 0 0) pattern) black
                    , \_ -> assertColorEqual (patternAt (point 0 0 1) pattern) black
                    , \_ -> assertColorEqual (patternAt (point 0.708 0 0.708) pattern) black
                    ]
                    ()
            )
        , test "Checkers should repeat in x"
            (\_ ->
                let
                    pattern =
                        checkersPattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0.99 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 1.01 0 0) pattern) black
                    ]
                    ()
            )
        , test "Checkers should repeat in y"
            (\_ ->
                let
                    pattern =
                        checkersPattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 0.99 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 1.01 0) pattern) black
                    ]
                    ()
            )
        , test "Checkers should repeat in z"
            (\_ ->
                let
                    pattern =
                        checkersPattern white black
                in
                Expect.all
                    [ \_ -> assertColorEqual (patternAt (point 0 0 0) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 0 0.99) pattern) white
                    , \_ -> assertColorEqual (patternAt (point 0 0 1.01) pattern) black
                    ]
                    ()
            )
        ]


assertColorEqual : Color -> Color -> Expect.Expectation
assertColorEqual col1 col2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute epsilon) col1.red col2.red
        , \_ -> Expect.within (Expect.Absolute epsilon) col1.green col2.green
        , \_ -> Expect.within (Expect.Absolute epsilon) col1.blue col2.blue
        ]
        ()
