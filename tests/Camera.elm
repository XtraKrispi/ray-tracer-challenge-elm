module Camera exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Lib.Camera exposing (camera, rayForPixel, setTransform)
import Lib.Canvas exposing (pixelAt)
import Lib.Color exposing (color)
import Lib.Matrix exposing (identityMatrix, multiply)
import Lib.Matrix.Transformation exposing (RotationAmount(..), rotationY, translation, viewTransform)
import Lib.Tuple exposing (Tuple, point, vector)
import Lib.World exposing (defaultWorld)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Camera Tests"
        [ test "Constructing a camera"
            (\_ ->
                let
                    hsize =
                        160

                    vsize =
                        120

                    fieldOfView =
                        pi / 2

                    c =
                        camera hsize vsize fieldOfView
                in
                Expect.all
                    [ \_ -> Expect.equal c.hsize 160
                    , \_ -> Expect.equal c.vsize 120
                    , \_ -> Expect.within (Expect.Absolute 0.00001) c.fieldOfView (pi / 2)
                    , \_ -> Expect.equal (Lib.Matrix.equal c.transform identityMatrix) True
                    ]
                    ()
            )
        , test "The pixel size for a horizontal canvas"
            (\_ ->
                let
                    c =
                        camera 200 125 (pi / 2)
                in
                Expect.within (Expect.Absolute 0.00001) c.pixelSize 0.01
            )
        , test "The pixel size for a vertical canvas"
            (\_ ->
                let
                    c =
                        camera 125 200 (pi / 2)
                in
                Expect.within (Expect.Absolute 0.00001) c.pixelSize 0.01
            )
        , test "Constructing a ray through the center of the canvas"
            (\_ ->
                let
                    c =
                        camera 201 101 (pi / 2)

                    r =
                        rayForPixel c 100 50
                in
                Expect.all
                    [ \_ -> assertEqualTuple r.origin (point 0 0 0)
                    , \_ -> assertEqualTuple r.direction (vector 0 0 -1)
                    ]
                    ()
            )
        , test "Constructing a ray through a corner of the canvas"
            (\_ ->
                let
                    c =
                        camera 201 101 (pi / 2)

                    r =
                        rayForPixel c 0 0
                in
                Expect.all
                    [ \_ -> assertEqualTuple r.origin (point 0 0 0)
                    , \_ -> assertEqualTuple r.direction (vector 0.66519 0.33259 -0.66851)
                    ]
                    ()
            )
        , test "Constructing a ray when the camera is transformed"
            (\_ ->
                let
                    c =
                        camera 201 101 (pi / 2)
                            |> setTransform (multiply (rotationY (Radians (pi / 4))) (translation 0 -2 5))

                    r =
                        rayForPixel c 100 50
                in
                Expect.all
                    [ \_ -> assertEqualTuple r.origin (point 0 2 -5)
                    , \_ -> assertEqualTuple r.direction (vector (sqrt 2 / 2) 0 -(sqrt 2 / 2))
                    ]
                    ()
            )
        , test "Rendering a world with a camera"
            (\_ ->
                let
                    w =
                        defaultWorld

                    c =
                        camera 11 11 (pi / 2)
                            |> setTransform (viewTransform from to up)

                    from =
                        point 0 0 -5

                    to =
                        point 0 0 0

                    up =
                        vector 0 1 0

                    image =
                        Lib.Camera.render c w
                in
                image
                    |> pixelAt ( 5, 5 )
                    |> Maybe.map (Lib.Color.equal (color 0.38066 0.47583 0.2855))
                    |> Expect.equal (Just True)
            )
        ]


assertEqualTuple : Tuple -> Tuple -> Expect.Expectation
assertEqualTuple t1 t2 =
    Expect.equal (Lib.Tuple.equal t1 t2) True
