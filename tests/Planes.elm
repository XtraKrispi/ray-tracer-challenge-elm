module Planes exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Intersection exposing (localIntersections)
import Lib.Object exposing (Id(..), localNormalAt, plane)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (Tuple, point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Plane Tests"
        [ test "The normal of a plane is constant everywhere"
            (\_ ->
                let
                    p =
                        plane (Id 1)

                    n1 =
                        localNormalAt (point 0 0 0) p

                    n2 =
                        localNormalAt (point 10 0 -10) p

                    n3 =
                        localNormalAt (point -5 0 150) p
                in
                Expect.all
                    [ \v -> assertEqualTuple n1 v
                    , \v -> assertEqualTuple n2 v
                    , \v -> assertEqualTuple n3 v
                    ]
                    (vector 0 1 0)
            )
        , test "Intersect with a ray parallel to the plane"
            (\_ ->
                let
                    p =
                        plane (Id 1)

                    r =
                        Ray (point 0 10 0) (vector 0 0 1)

                    xs =
                        localIntersections r p
                in
                Expect.equal xs []
            )
        , test "Intersect with a coplanar ray"
            (\_ ->
                let
                    p =
                        plane (Id 1)

                    r =
                        Ray (point 0 0 0) (vector 0 0 1)

                    xs =
                        localIntersections r p
                in
                Expect.equal xs []
            )
        , test "A ray intersecting a plane from above"
            (\_ ->
                let
                    p =
                        plane (Id 1)

                    r =
                        Ray (point 0 1 0) (vector 0 -1 0)

                    xs =
                        localIntersections r p
                in
                Expect.equal xs [ { t = 1, object = p } ]
            )
        , test "A ray intersecting a plane from below"
            (\_ ->
                let
                    p =
                        plane (Id 1)

                    r =
                        Ray (point 0 -1 0) (vector 0 1 0)

                    xs =
                        localIntersections r p
                in
                Expect.equal xs [ { t = 1, object = p } ]
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
