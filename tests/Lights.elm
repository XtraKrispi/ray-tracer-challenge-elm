module Lights exposing (..)

import Expect
import Lib.Color exposing (color)
import Lib.Light exposing (pointLight)
import Lib.Tuple exposing (point)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Light Tests"
        [ test "A point light has a position and intensity"
            (\_ ->
                let
                    intensity =
                        color 1 1 1

                    position =
                        point 0 0 0

                    light =
                        pointLight position intensity
                in
                Expect.all
                    [ \_ -> Expect.equal light.position position
                    , \_ -> Expect.equal light.intensity intensity
                    ]
                    ()
            )
        ]
