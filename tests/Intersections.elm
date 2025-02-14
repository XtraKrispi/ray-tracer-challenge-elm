module Intersections exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Intersection exposing (Intersection, hit, intersections, prepareComputations, schlick)
import Lib.Material exposing (material)
import Lib.Matrix.Transformation exposing (scaling, translation)
import Lib.Object exposing (Id(..), glassSphere, plane, setTransform, sphere)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (Tuple, point, vector)
import List.Extra
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
                            (prepareComputations r [ i ] i)

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
                            (prepareComputations r [ i ] i)

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
                            (prepareComputations r [ i ] i)

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
                        prepareComputations r [ i ] i
                in
                Expect.all
                    [ \_ -> Expect.equal (comps.overPoint.z < -epsilon / 2) True
                    , \_ -> Expect.equal (comps.point.z > comps.overPoint.z) True
                    ]
                    ()
            )
        , test "Precomputing the reflection vector"
            (\_ ->
                let
                    shape =
                        plane (Id 1)

                    r =
                        Ray (point 0 1 -1) (vector 0 -(sqrt (2 / 2)) (sqrt (2 / 2)))

                    i =
                        Intersection (sqrt 2) shape

                    comps =
                        prepareComputations r [ i ] i
                in
                assertEqualTuple comps.reflectv (vector 0 (sqrt (2 / 2)) (sqrt (2 / 2)))
            )
        , describe "Finding n1 and n2 at various intersections"
            (List.map refractionIntersectionsTest refractionPoints)
        , test "The under point is offset below the surface"
            (\_ ->
                let
                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    shape =
                        glassSphere (Id 1)
                            |> Lib.Object.setTransform (translation 0 0 1)

                    i =
                        Intersection 5 shape

                    comps =
                        prepareComputations r [ i ] i
                in
                Expect.all
                    [ \_ -> Expect.equal (comps.underPoint.z > (epsilon / 2)) True
                    , \_ -> Expect.equal (comps.point.z < comps.underPoint.z) True
                    ]
                    ()
            )
        , test "The Schlick approximation under total internal reflection"
            (\_ ->
                let
                    shape =
                        glassSphere (Id 1)

                    r =
                        Ray (point 0 0 (sqrt 2 / 2)) (vector 0 1 0)

                    xs =
                        [ Intersection -(sqrt 2 / 2) shape, Intersection (sqrt 2 / 2) shape ]

                    comps =
                        prepareComputations r xs (Intersection (sqrt 2 / 2) shape)
                in
                Expect.within (Expect.Absolute epsilon) (schlick comps) 1
            )
        , test "The Schlick approximation with a perpendicular viewing angle"
            (\_ ->
                let
                    shape =
                        glassSphere (Id 1)

                    r =
                        Ray (point 0 0 0) (vector 0 1 0)

                    xs =
                        [ Intersection -1 shape, Intersection 1 shape ]

                    comps =
                        prepareComputations r xs (Intersection 1 shape)
                in
                Expect.within (Expect.Absolute epsilon) (schlick comps) 0.04
            )
        , test "The Schlick approximation with small angle and n2 > n1"
            (\_ ->
                let
                    shape =
                        glassSphere (Id 1)

                    r =
                        Ray (point 0 0.99 -2) (vector 0 0 1)

                    xs =
                        [ Intersection 1.8589 shape ]

                    comps =
                        prepareComputations r xs (Intersection 1.8589 shape)
                in
                Expect.within (Expect.Absolute epsilon) (schlick comps) 0.48873
            )
        ]


refractionPoints : List { index : Int, n1 : Float, n2 : Float }
refractionPoints =
    [ { index = 0, n1 = 1, n2 = 1.5 }
    , { index = 1, n1 = 1.5, n2 = 2 }
    , { index = 2, n1 = 2, n2 = 2.5 }
    , { index = 3, n1 = 2.5, n2 = 2.5 }
    , { index = 4, n1 = 2.5, n2 = 1.5 }
    , { index = 5, n1 = 1.5, n2 = 1 }
    ]


refractionIntersectionsTest : { index : Int, n1 : Float, n2 : Float } -> Test
refractionIntersectionsTest pt =
    test ("Finding n1 and n2 at index " ++ String.fromInt pt.index)
        (\_ ->
            let
                a =
                    glassSphere (Id 1)
                        |> Lib.Object.setTransform (scaling 2 2 2)
                        |> Lib.Object.setMaterial { material | refractiveIndex = 1.5 }

                b =
                    glassSphere (Id 2)
                        |> Lib.Object.setTransform (translation 0 0 -0.25)
                        |> Lib.Object.setMaterial { material | refractiveIndex = 2 }

                c =
                    glassSphere (Id 3)
                        |> Lib.Object.setTransform (translation 0 0 0.25)
                        |> Lib.Object.setMaterial { material | refractiveIndex = 2.5 }

                r =
                    Ray (point 0 0 -4) (vector 0 0 1)

                xs =
                    [ { t = 2, object = a }
                    , { t = 2.75, object = b }
                    , { t = 3.25, object = c }
                    , { t = 4.75, object = b }
                    , { t = 5.25, object = c }
                    , { t = 6, object = a }
                    ]
            in
            case List.Extra.getAt pt.index xs of
                Just x ->
                    let
                        comps =
                            prepareComputations r xs x
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute epsilon) comps.n1 pt.n1
                        , \_ -> Expect.within (Expect.Absolute epsilon) comps.n2 pt.n2
                        ]
                        ()

                Nothing ->
                    Expect.fail "This shouldn't happen"
        )


assertEqualTuple : Tuple -> Tuple -> Expect.Expectation
assertEqualTuple t1 t2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute epsilon) t1.x t2.x
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.y t2.y
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.z t2.z
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.w t2.w
        ]
        ()
