module World exposing (..)

import Expect exposing (Expectation)
import Lib.Color exposing (Color, black, color, white)
import Lib.Intersection exposing (Intersection, prepareComputations)
import Lib.Light exposing (pointLight)
import Lib.Material exposing (material)
import Lib.Matrix.Transformation exposing (scaling, translation)
import Lib.Object exposing (Id(..), setMaterial, setTransform, sphere)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (point, vector)
import Lib.World exposing (addLight, addObject, colorAt, defaultWorld, intersectWorld, isShadowed, shadeHit, world)
import List.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "World Tests"
        [ test "Creating a world"
            (\_ ->
                let
                    w =
                        world
                in
                Expect.equal (List.isEmpty w.objects) True
            )
        , test "The default world"
            (\_ ->
                let
                    light =
                        pointLight (point -10 10 -10) white

                    s1 =
                        sphere (Id 1)
                            |> setMaterial
                                { material
                                    | color = color 0.8 1 0.6
                                    , diffuse = 0.7
                                    , specular = 0.2
                                }

                    s2 =
                        sphere (Id 1)
                            |> setTransform (scaling 0.5 0.5 0.5)
                in
                Expect.all
                    [ \w -> Expect.equal w.lights [ light ]
                    , \w -> Expect.equal (List.member s1 w.objects) True
                    , \w -> Expect.equal (List.member s2 w.objects) True
                    ]
                    defaultWorld
            )
        , test "Intersect a world with a ray"
            (\_ ->
                let
                    w =
                        defaultWorld

                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    xs =
                        intersectWorld w r
                in
                Expect.equalLists (List.map .t xs)
                    [ 4, 4.5, 5.5, 6 ]
            )
        , test "Shading an intersection"
            (\_ ->
                let
                    w =
                        defaultWorld

                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)
                in
                case
                    w.objects
                        |> List.head
                of
                    Just shape ->
                        let
                            i =
                                Intersection 4 shape

                            comps =
                                prepareComputations r i

                            c =
                                shadeHit w comps
                        in
                        Expect.equal (Lib.Color.equal c (color 0.38066 0.47583 0.2855)) True

                    Nothing ->
                        Expect.fail "Ruh-roh"
            )
        , test "Shading an intersection from the inside"
            (\_ ->
                let
                    w =
                        { defaultWorld | lights = [ pointLight (point 0 0.25 0) white ] }

                    r =
                        Ray (point 0 0 0) (vector 0 0 1)
                in
                case
                    w.objects
                        |> List.Extra.getAt 1
                of
                    Just shape ->
                        let
                            i =
                                Intersection 0.5 shape

                            comps =
                                prepareComputations r i

                            c =
                                shadeHit w comps
                        in
                        Expect.equal (Lib.Color.equal c (color 0.90498 0.90498 0.90498)) True

                    Nothing ->
                        Expect.fail "Ruh-roh"
            )
        , test "The color when a ray misses"
            (\_ ->
                let
                    w =
                        defaultWorld

                    r =
                        Ray (point 0 0 -5) (vector 0 1 0)

                    c =
                        colorAt w r
                in
                assertColorMatches c black
            )
        , test "The color when a ray hits"
            (\_ ->
                let
                    w =
                        defaultWorld

                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)

                    c =
                        colorAt w r
                in
                assertColorMatches c (color 0.38066 0.47583 0.2855)
            )
        , test "The color with an intersection behind the ray"
            (\_ ->
                let
                    w =
                        defaultWorld
                in
                case w.objects of
                    [ outer, inner ] ->
                        let
                            outerMaterial =
                                outer.material

                            newOuterMaterial =
                                { outerMaterial | ambient = 1 }

                            newOuter =
                                { outer | material = newOuterMaterial }

                            innerMaterial =
                                inner.material

                            newInnerMaterial =
                                { innerMaterial | ambient = 1 }

                            newInner =
                                { inner | material = newInnerMaterial }

                            newWorld =
                                { w | objects = [ newOuter, newInner ] }

                            r =
                                Ray (point 0 0 0.75) (vector 0 0 -1)

                            c =
                                colorAt newWorld r
                        in
                        assertColorMatches c newInnerMaterial.color

                    _ ->
                        Expect.fail "This wasn't supposed to happen"
            )
        , test "There is no shadow when nothing is collinear with point and light"
            (\_ ->
                let
                    w =
                        defaultWorld

                    p =
                        point 0 10 0
                in
                Expect.equal (isShadowed w p) False
            )
        , test "The shadow when an object is between the point and the light"
            (\_ ->
                let
                    w =
                        defaultWorld

                    p =
                        point 10 -10 10
                in
                Expect.equal (isShadowed w p) True
            )
        , test "There is no shadow when an object is behind the light"
            (\_ ->
                let
                    w =
                        defaultWorld

                    p =
                        point -20 20 -20
                in
                Expect.equal (isShadowed w p) False
            )
        , test "There is no shadow when an object is behind the point"
            (\_ ->
                let
                    w =
                        defaultWorld

                    p =
                        point -2 2 -2
                in
                Expect.equal (isShadowed w p) False
            )
        , test "shade_hit() is given an intersection in shadow"
            (\_ ->
                let
                    s1 =
                        sphere (Id 0)

                    s2 =
                        sphere (Id 1)
                            |> setTransform (translation 0 0 10)

                    w =
                        world
                            |> addLight (pointLight (point 0 0 -10) white)
                            |> addObject s1
                            |> addObject s2

                    r =
                        Ray (point 0 0 5) (vector 0 0 1)

                    i =
                        Intersection 4 s2

                    comps =
                        prepareComputations r i

                    c =
                        shadeHit w comps
                in
                assertColorMatches c (color 0.1 0.1 0.1)
            )
        ]


assertColorMatches : Color -> Color -> Expectation
assertColorMatches c1 c2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute 0.0001) c1.red c2.red
        , \_ -> Expect.within (Expect.Absolute 0.0001) c1.green c2.green
        , \_ -> Expect.within (Expect.Absolute 0.0001) c1.blue c2.blue
        ]
        ()
