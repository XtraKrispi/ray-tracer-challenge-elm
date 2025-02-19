module Cubes exposing (..)

import Expect
import Lib exposing (epsilon)
import Lib.Intersection exposing (localIntersections)
import Lib.Object exposing (Id(..), cube, localNormalAt)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (Tuple, point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cube Tests"
        [ [ { description = "Ray intersection +x"
            , origin = point 5 0.5 0
            , direction = vector -1 0 0
            , t1 = 4
            , t2 = 6
            }
          , { description = "Ray intersection -x"
            , origin = point -5 0.5 0
            , direction = vector 1 0 0
            , t1 = 4
            , t2 = 6
            }
          , { description = "Ray intersection +y"
            , origin = point 0.5 5 0
            , direction = vector 0 -1 0
            , t1 = 4
            , t2 = 6
            }
          , { description = "Ray intersection -y"
            , origin = point 0.5 -5 0
            , direction = vector 0 1 0
            , t1 = 4
            , t2 = 6
            }
          , { description = "Ray intersection +z"
            , origin = point 0.5 0 5
            , direction = vector 0 0 -1
            , t1 = 4
            , t2 = 6
            }
          , { description = "Ray intersection -z"
            , origin = point 0.5 0 -5
            , direction = vector 0 0 1
            , t1 = 4
            , t2 = 6
            }
          , { description = "Ray intersection inside"
            , origin = point 0 0.5 0
            , direction = vector 0 0 1
            , t1 = -1
            , t2 = 1
            }
          ]
            |> List.map cubeIntersectionTest
            |> describe "Ray Intersections"
        , [ { description = "#1"
            , origin = point -2 0 0
            , direction = vector 0.2673 0.5345 0.8018
            }
          , { description = "#2"
            , origin = point 0 -2 0
            , direction = vector 0.8018 0.2673 0.5345
            }
          , { description = "#3"
            , origin = point 0 0 -2
            , direction = vector 0.5345 0.8018 0.2673
            }
          , { description = "#4"
            , origin = point 2 0 2
            , direction = vector 0 0 -1
            }
          , { description = "#5"
            , origin = point 0 2 2
            , direction = vector 0 -1 0
            }
          , { description = "#6"
            , origin = point 2 2 0
            , direction = vector -1 0 0
            }
          ]
            |> List.map rayMissesCubeTest
            |> describe "Ray Misses Cube"
        , [ ( point 1 0.5 -0.8, vector 1 0 0 )
          , ( point -1 -0.2 0.9, vector -1 0 0 )
          , ( point -0.4 1 -0.1, vector 0 1 0 )
          , ( point 0.3 -1 -0.7, vector 0 -1 0 )
          , ( point -0.6 0.3 1, vector 0 0 1 )
          , ( point 0.4 0.4 -1, vector 0 0 -1 )
          , ( point 1 1 1, vector 1 0 0 )
          , ( point -1 -1 -1, vector -1 0 0 )
          ]
            |> List.indexedMap (\i ( p, n ) -> { description = String.fromInt i, point = p, normal = n })
            |> List.map localNormalTest
            |> describe "The normal on the surface of a cube"
        ]


cubeIntersectionTest : { description : String, origin : Tuple, direction : Tuple, t1 : Float, t2 : Float } -> Test
cubeIntersectionTest config =
    test config.description
        (\_ ->
            let
                c =
                    cube (Id 1)

                r =
                    Ray config.origin config.direction

                xs =
                    localIntersections r c
            in
            Expect.all
                [ \_ -> Expect.equal (List.length xs) 2
                , \_ -> Expect.equal (xs |> List.map .t) [ config.t1, config.t2 ]
                ]
                ()
        )


rayMissesCubeTest : { description : String, origin : Tuple, direction : Tuple } -> Test
rayMissesCubeTest { description, origin, direction } =
    test description
        (\_ ->
            let
                c =
                    cube (Id 1)

                r =
                    Ray origin direction

                xs =
                    localIntersections r c
            in
            Expect.equal (List.length xs) 0
        )


localNormalTest : { description : String, point : Tuple, normal : Tuple } -> Test
localNormalTest d =
    test d.description
        (\_ ->
            let
                c =
                    cube (Id 1)

                normal =
                    localNormalAt d.point c
            in
            assertTupleEqual normal d.normal
        )


assertTupleEqual : Tuple -> Tuple -> Expect.Expectation
assertTupleEqual t1 t2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute epsilon) t1.x t2.x
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.y t2.y
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.z t2.z
        , \_ -> Expect.within (Expect.Absolute epsilon) t1.w t2.w
        ]
        ()
