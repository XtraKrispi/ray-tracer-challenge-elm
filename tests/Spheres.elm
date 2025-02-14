module Spheres exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Intersection exposing (intersections)
import Lib.Material exposing (material)
import Lib.Matrix exposing (identityMatrix, multiply)
import Lib.Matrix.Transformation exposing (RotationAmount(..), rotationZ, scaling, translation)
import Lib.Object exposing (Id(..), glassSphere, normalAt, setMaterial, setTransform, sphere, testShape)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (Tuple, normalize, point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Sphere Tests"
        [ test "Intersecting a scaled sphere with a ray"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    s =
                        setTransform (scaling 2 2 2) (sphere (Id 1))

                    xs =
                        intersections r s
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length xs) 2
                    , \_ -> Expect.equal (List.map .t xs) [ 3, 7 ]
                    ]
                    ()
            )
        , test "Intersecting a translated sphere with a ray"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    s =
                        setTransform (translation 5 0 0) (sphere (Id 1))

                    xs =
                        intersections r s
                in
                Expect.equal (List.length xs) 0
            )
        , test "The normal on a sphere at a point on the x axis"
            (\_ ->
                let
                    s =
                        sphere (Id 1)

                    n =
                        normalAt (point 1 0 0) s
                in
                Expect.equal n (vector 1 0 0)
            )
        , test "The normal on a sphere at a point on the y axis"
            (\_ ->
                let
                    s =
                        sphere (Id 1)

                    n =
                        normalAt (point 0 1 0) s
                in
                Expect.equal n (vector 0 1 0)
            )
        , test "The normal on a sphere at a point on the z axis"
            (\_ ->
                let
                    s =
                        sphere (Id 1)

                    n =
                        normalAt (point 0 0 1) s
                in
                Expect.equal n (vector 0 0 1)
            )
        , test "The normal on a sphere at a nonaxial point"
            (\_ ->
                let
                    s =
                        sphere (Id 1)

                    n =
                        normalAt (point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)) s
                in
                assertEqualTuple n (vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
            )
        , test "The normal is a normalized vector"
            (\_ ->
                let
                    s =
                        sphere (Id 1)

                    n =
                        normalAt (point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)) s
                in
                assertEqualTuple n (normalize n)
            )
        , test "Computing the normal on a translated sphere"
            (\_ ->
                let
                    s =
                        sphere (Id 1)
                            |> setTransform (translation 0 1 0)

                    n =
                        normalAt (point 0 1.70711 -0.70711) s
                in
                assertEqualTuple n (vector 0 0.70711 -0.70711)
            )
        , test "Computing the normal on a transformed sphere"
            (\_ ->
                let
                    s =
                        sphere (Id 1)
                            |> setTransform (multiply (scaling 1 0.5 1) (rotationZ (Radians (pi / 5))))

                    n =
                        normalAt (point 0 (sqrt 2 / 2) -(sqrt 2 / 2)) s
                in
                assertEqualTuple n (vector 0 0.97014 -0.24254)
            )
        , test "A sphere has a default material"
            (\_ ->
                let
                    s =
                        sphere (Id 1)
                in
                Expect.equal s.material material
            )
        , test "A sphere may be assigned a material"
            (\_ ->
                let
                    m =
                        { material | ambient = 1 }

                    s =
                        sphere (Id 1)
                            |> setMaterial m
                in
                Expect.equal s.material m
            )
        , test "A helper for producing a sphere with a glassy material"
            (\_ ->
                Expect.all
                    [ \s -> Expect.equal s.transform identityMatrix
                    , \s -> Expect.within (Expect.Absolute epsilon) s.material.transparency 1
                    , \s -> Expect.within (Expect.Absolute epsilon) s.material.refractiveIndex 1.5
                    ]
                    (glassSphere (Id 1))
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
