module Rays exposing (..)

import Expect
import Lib.Matrix.Transformation exposing (scaling, translation)
import Lib.Object exposing (Id(..))
import Lib.Ray exposing (Ray, position)
import Lib.Tuple exposing (point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ray Tests"
        [ test "Computing a point from a distance"
            (\_ ->
                let
                    r =
                        Ray (point 2 3 4) (vector 1 0 0)
                in
                Expect.all
                    [ \_ -> Expect.equal (position r 0) (point 2 3 4)
                    , \_ -> Expect.equal (position r 1) (point 3 3 4)
                    , \_ -> Expect.equal (position r -1) (point 1 3 4)
                    , \_ -> Expect.equal (position r 2.5) (point 4.5 3 4)
                    ]
                    ()
            )
        , test "Translating a ray"
            (\_ ->
                let
                    r =
                        Ray (point 1 2 3) (vector 0 1 0)

                    m =
                        translation 3 4 5

                    r2 =
                        Lib.Ray.transform r m
                in
                Expect.equal r2 (Ray (point 4 6 8) (vector 0 1 0))
            )
        , test "Scaling a ray"
            (\_ ->
                let
                    r =
                        Ray (point 1 2 3) (vector 0 1 0)

                    m =
                        scaling 2 3 4

                    r2 =
                        Lib.Ray.transform r m
                in
                Expect.equal r2 (Ray (point 2 6 12) (vector 0 3 0))
            )
        ]
