module Shapes exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Intersection exposing (intersections)
import Lib.Material exposing (material)
import Lib.Matrix exposing (identityMatrix, multiply)
import Lib.Matrix.Transformation exposing (RotationAmount(..), rotationZ, scaling, translation)
import Lib.Object exposing (Id(..), normalAt, setMaterial, setTransform, sphere, testShape)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (Tuple, normalize, point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Shape Tests"
        [ test "The default transformation"
            (\_ ->
                let
                    s =
                        testShape (Id 1)
                in
                Expect.equal s.transform identityMatrix
            )
        , test "Assigning a transformation"
            (\_ ->
                let
                    s =
                        testShape (Id 1)

                    t =
                        translation 2 3 4
                in
                Expect.equal (setTransform t s).transform t
            )
        , test "The default material"
            (\_ ->
                let
                    s =
                        testShape (Id 1)
                in
                Expect.equal s.material material
            )
        , test "Assigning a material"
            (\_ ->
                let
                    s =
                        testShape (Id 1)
                            |> setMaterial { material | ambient = 1 }
                in
                Expect.equal s.material.ambient 1
            )
        , test "Computing the normal on a translated shape"
            (\_ ->
                let
                    s =
                        testShape (Id 1)
                            |> setTransform (translation 0 1 0)

                    n =
                        normalAt (point 0 1.70711 -0.70711) s
                in
                assertEqualTuple n (vector 0 0.70711 -0.70711)
            )
        , test "Computing the normal on a transformed shape"
            (\_ ->
                let
                    s =
                        testShape (Id 1)
                            |> setTransform (multiply (scaling 1 0.5 1) (rotationZ (Radians (pi / 5))))

                    n =
                        normalAt (point 0 (sqrt 2 / 2) -(sqrt 2 / 2)) s
                in
                assertEqualTuple n (vector 0 0.97014 -0.24254)
            )
        ]


assertEqualTuple : Tuple -> Tuple -> Expect.Expectation
assertEqualTuple t1 t2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute epsilon) t1.x t2.x
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.y t2.y
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.z t2.z
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.w t2.w
        ]
        ()
