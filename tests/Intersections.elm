module Intersections exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Intersection exposing (Intersection, hit, intersections, prepareComputations)
import Lib.Matrix.Transformation exposing (translation)
import Lib.Object exposing (Id(..), setTransform, sphere)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Intersection Tests"
        [ test "A ray intersects a sphere at two points"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    s =
                        sphere (Id 1)
                in
                Expect.equalLists [ 4, 6 ] (List.map .t <| intersections r s)
            )
        , test "A ray misses a sphere"
            (\_ ->
                let
                    r =
                        Ray (point 0 2 -5) (vector 0 0 1)

                    s =
                        sphere (Id 1)
                in
                Expect.equalLists [] (intersections r s)
            )
        , test "A ray originates inside a sphere"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 0) (vector 0 0 1)

                    s =
                        sphere (Id 1)
                in
                Expect.equalLists [ -1, 1 ] (List.map .t <| intersections r s)
            )
        , test "A sphere is behind a ray"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 5) (vector 0 0 1)

                    s =
                        sphere (Id 1)
                in
                Expect.equalLists [ -6, -4 ] (List.map .t <| intersections r s)
            )
        , test "Intersect sets the object on the intersection"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    s =
                        sphere (Id 10)
                in
                Expect.equalLists [ s, s ] (List.map .object <| intersections r s)
            )
        , test "The hit, when all intersections have positive t"
            (\_ ->
                let
                    s =
                        sphere (Id 10)

                    i1 =
                        Intersection 1 s

                    i2 =
                        Intersection 2 s
                in
                Expect.equal (hit [ i1, i2 ]) (Just i1)
            )
        , test "The hit, when some intersections have negative t"
            (\_ ->
                let
                    s =
                        sphere (Id 10)

                    i1 =
                        Intersection -1 s

                    i2 =
                        Intersection 1 s
                in
                Expect.equal (hit [ i2, i1 ]) (Just i2)
            )
        , test "The hit, when all intersections have negative t"
            (\_ ->
                let
                    s =
                        sphere (Id 10)

                    i1 =
                        Intersection -2 s

                    i2 =
                        Intersection -1 s
                in
                Expect.equal (hit [ i2, i1 ]) Nothing
            )
        , test "The hit is always the lowest nonnegative intersection"
            (\_ ->
                let
                    s =
                        sphere (Id 10)

                    i1 =
                        Intersection 5 s

                    i2 =
                        Intersection 7 s

                    i3 =
                        Intersection -3 s

                    i4 =
                        Intersection 2 s
                in
                Expect.equal (hit [ i1, i2, i3, i4 ]) (Just i4)
            )
        , test "Precomputing the state of an intersection"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    shape =
                        sphere (Id 1)

                    mI =
                        intersections r shape |> List.head
                in
                case mI of
                    Just i ->
                        Expect.all
                            [ \comps -> Expect.equal comps.t i.t
                            , \comps -> Expect.equal comps.object i.object
                            , \comps -> Expect.equal comps.point (point 0 0 -1)
                            , \comps -> Expect.equal comps.eyev (vector 0 0 -1)
                            , \comps -> Expect.equal comps.normalv (vector 0 0 -1)
                            ]
                            (prepareComputations r i)

                    Nothing ->
                        Expect.fail "There were supposed to be intersections here"
            )
        , test "The hit, when an intersection occurs on the outside"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    shape =
                        sphere (Id 1)

                    mI =
                        intersections r shape |> List.head
                in
                case mI of
                    Just i ->
                        Expect.all
                            [ \comps -> Expect.equal comps.inside False
                            ]
                            (prepareComputations r i)

                    Nothing ->
                        Expect.fail "There were supposed to be intersections here"
            )
        , test "The hit, when an intersection occurs on the inside"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 0) (vector 0 0 1)

                    shape =
                        sphere (Id 1)

                    mI =
                        intersections r shape
                            |> List.reverse
                            |> List.head
                in
                case mI of
                    Just i ->
                        Expect.all
                            [ \comps -> Expect.equal comps.point (point 0 0 1)
                            , \comps -> Expect.equal comps.eyev (vector 0 0 -1)
                            , \comps -> Expect.equal comps.inside True
                            , \comps -> Expect.equal comps.normalv (vector 0 0 -1)
                            ]
                            (prepareComputations r i)

                    Nothing ->
                        Expect.fail "There were supposed to be intersections here"
            )
        , test "The hit should offset the point"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    shape =
                        sphere (Id 1) |> setTransform (translation 0 0 1)

                    i =
                        Intersection 5 shape

                    comps =
                        prepareComputations r i
                in
                Expect.all
                    [ \_ -> Expect.equal (comps.overPoint.z < -epsilon / 2) True
                    , \_ -> Expect.equal (comps.point.z > comps.overPoint.z) True
                    ]
                    ()
            )
        ]



{-
   Scenario: The hit should offset the point
   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
   And shape ← sphere() with:
   | transform | translation(0, 0, 1) |
   And i ← intersection(5, shape)
   When comps ← prepare_computations(i, r)
   Then comps.over_point.z < -EPSILON/2
   And comps.point.z > comps.over_point.z
-}
