module Canvas exposing (..)

import Dict
import Expect
import Lib.Canvas exposing (canvas, pixelAt, writePixel)
import Lib.Color exposing (color)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Canvas Operations"
        [ test "Creating a canvas"
            (\_ ->
                let
                    c =
                        canvas (always (color 0 0 0)) 10 20
                in
                Expect.all
                    [ \{ width } -> Expect.equal width 10
                    , \{ height } -> Expect.equal height 20
                    , \{ pixels } ->
                        Expect.equal
                            (Dict.filter (\_ v -> v /= color 0 0 0) pixels
                                |> Dict.size
                            )
                            0
                    ]
                    c
            )
        , test "Writing pixels to a canvas"
            (\_ ->
                let
                    c =
                        canvas (always (color 0 0 0)) 10 20

                    red =
                        color 1 0 0

                    c_ =
                        writePixel ( 2, 3 ) red c
                in
                Expect.equal (pixelAt ( 2, 3 ) c_) (Just red)
            )
        ]
