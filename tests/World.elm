module World exposing (..)

import Expect exposing (Expectation)
import Lib exposing (epsilon)
import Lib.Color exposing (Color, black, color, white)
import Lib.Intersection exposing (Intersection, prepareComputations)
import Lib.Light exposing (pointLight)
import Lib.Material exposing (material)
import Lib.Matrix.Transformation exposing (scaling, translation)
import Lib.Object exposing (Id(..), plane, setMaterial, setTransform, sphere)
import Lib.Pattern exposing (testPattern)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (point, vector)
import Lib.World exposing (addLight, addObject, colorAt, defaultWorld, emptyWorld, indexedMapObjects, intersectWorld, isShadowed, mapObjects, reflectedColor, refractedColor, shadeHit)
import List.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "World Tests"
        [ test "Creating a world"
            (\_ ->
                let
                    w =
                        emptyWorld
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
                        sphere (Id 2)
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
                                prepareComputations r [] i

                            c =
                                shadeHit w 5 comps
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
                                prepareComputations r [] i

                            c =
                                shadeHit w 5 comps
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
                        colorAt w 5 r
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
                        colorAt w 5 r
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
                                colorAt newWorld 5 r
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
                        emptyWorld
                            |> addLight (pointLight (point 0 0 -10) white)
                            |> addObject s1
                            |> addObject s2

                    r =
                        Ray (point 0 0 5) (vector 0 0 1)

                    i =
                        Intersection 4 s2

                    comps =
                        prepareComputations r [ i ] i

                    c =
                        shadeHit w 5 comps
                in
                assertColorMatches c (color 0.1 0.1 0.1)
            )
        , test "The reflected color for a nonreflective material"
            (\_ ->
                let
                    w =
                        defaultWorld

                    r =
                        Ray (point 0 0 0) (vector 0 0 1)

                    shape =
                        sphere (Id 1)
                            |> setTransform (scaling 0.5 0.5 0.5)

                    i =
                        Intersection 1 shape

                    comps =
                        prepareComputations r [ i ] i

                    color =
                        reflectedColor w 5 comps
                in
                assertColorMatches color black
            )
        , test "The reflected color for a reflective material"
            (\_ ->
                let
                    shape =
                        plane (Id 10)
                            |> Lib.Object.setTransform (translation 0 -1 0)
                            |> Lib.Object.setMaterial { material | reflective = 0.5 }

                    w =
                        defaultWorld
                            |> Lib.World.addObject shape

                    r =
                        Ray (point 0 0 -3) (vector 0 -(sqrt 2 / 2) (sqrt 2 / 2))

                    i =
                        Intersection (sqrt 2) shape

                    comps =
                        prepareComputations r [ i ] i

                    c =
                        reflectedColor w 5 comps
                in
                assertColorMatches c (color 0.19032 0.2379 0.14274)
            )
        , test "shade_hit() with a reflective material"
            (\_ ->
                let
                    shape =
                        plane (Id 10)
                            |> Lib.Object.setTransform (translation 0 -1 0)
                            |> Lib.Object.setMaterial { material | reflective = 0.5 }

                    w =
                        defaultWorld
                            |> Lib.World.addObject shape

                    r =
                        Ray (point 0 0 -3) (vector 0 -(sqrt 2 / 2) (sqrt 2 / 2))

                    i =
                        Intersection (sqrt 2) shape

                    comps =
                        prepareComputations r [ i ] i

                    c =
                        shadeHit w 5 comps
                in
                assertColorMatches c (color 0.87677 0.92436 0.82918)
            )
        , test "color_at() with mutually reflective surfaces"
            (\_ ->
                let
                    w =
                        emptyWorld
                            |> addLight (pointLight (point 0 0 0) white)
                            |> addObject
                                (plane (Id 1)
                                    |> setMaterial { material | reflective = 1 }
                                    |> setTransform (translation 0 -1 0)
                                )
                            |> addObject
                                (plane (Id 2)
                                    |> setMaterial { material | reflective = 1 }
                                    |> setTransform (translation 0 1 0)
                                )

                    r =
                        Ray (point 0 0 0) (vector 0 1 0)

                    c =
                        colorAt w 5 r
                in
                Expect.equal c c
            )
        , test "The reflected color at the maximum recursive depth"
            (\_ ->
                let
                    shape =
                        plane (Id 10)
                            |> Lib.Object.setTransform (translation 0 -1 0)
                            |> Lib.Object.setMaterial { material | reflective = 0.5 }

                    w =
                        defaultWorld
                            |> Lib.World.addObject shape

                    r =
                        Ray (point 0 0 -3) (vector 0 -(sqrt 2 / 2) (sqrt 2 / 2))

                    i =
                        Intersection (sqrt 2) shape

                    comps =
                        prepareComputations r [] i

                    c =
                        reflectedColor w 0 comps
                in
                assertColorMatches c black
            )
        , test "The refracted color with an opaque surface"
            (\_ ->
                let
                    w =
                        defaultWorld

                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)
                in
                case w.objects of
                    [] ->
                        Expect.fail "Whoopsie"

                    shape :: _ ->
                        let
                            xs =
                                [ Intersection 4 shape, Intersection 6 shape ]

                            comps =
                                prepareComputations r xs (Intersection 4 shape)

                            c =
                                refractedColor w 5 comps
                        in
                        assertColorMatches c black
            )
        , test "The refracted color at the maximum recursive depth"
            (\_ ->
                let
                    w =
                        defaultWorld
                            |> indexedMapObjects
                                (\i o ->
                                    if i == 0 then
                                        o
                                            |> Lib.Object.setMaterial { material | transparency = 1, refractiveIndex = 1.5 }

                                    else
                                        o
                                )

                    r =
                        Ray (point 0 0 -5) (vector 0 0 1)
                in
                case w.objects of
                    [] ->
                        Expect.fail "Whoopsie"

                    shape :: _ ->
                        let
                            xs =
                                [ Intersection 4 shape, Intersection 6 shape ]

                            comps =
                                prepareComputations r xs (Intersection 4 shape)

                            c =
                                refractedColor w 0 comps
                        in
                        assertColorMatches c black
            )
        , test "The refracted color under total internal reflection"
            (\_ ->
                let
                    w =
                        defaultWorld
                            |> indexedMapObjects
                                (\i o ->
                                    if i == 0 then
                                        o
                                            |> Lib.Object.setMaterial { material | transparency = 1, refractiveIndex = 1.5 }

                                    else
                                        o
                                )

                    r =
                        Ray (point 0 0 (sqrt 2 / 2)) (vector 0 1 0)
                in
                case w.objects of
                    [] ->
                        Expect.fail "Whoopsie"

                    shape :: _ ->
                        let
                            xs =
                                [ Intersection -(sqrt 2 / 2) shape, Intersection (sqrt 2 / 2) shape ]

                            comps =
                                prepareComputations r xs (Intersection (sqrt 2 / 2) shape)

                            c =
                                refractedColor w 5 comps
                        in
                        assertColorMatches c black
            )
        , test "The refracted color with a refracted ray"
            (\_ ->
                let
                    w =
                        defaultWorld
                            |> indexedMapObjects
                                (\i o ->
                                    if i == 0 then
                                        o
                                            |> Lib.Object.setMaterial { material | ambient = 1, pattern = Just testPattern }

                                    else
                                        o |> Lib.Object.setMaterial { material | transparency = 1, refractiveIndex = 1.5 }
                                )

                    r =
                        Ray (point 0 0 0.1) (vector 0 1 0)
                in
                case w.objects of
                    [ a, b ] ->
                        let
                            {- intersections(-0.9899:A, -0.4899:B, 0.4899:B, 0.9899:A) -}
                            xs =
                                [ Intersection -0.9899 a, Intersection -0.4899 b, Intersection 0.4899 b, Intersection 0.9899 a ]

                            comps =
                                prepareComputations r xs (Intersection 0.4899 b)

                            c =
                                refractedColor w 5 comps
                        in
                        assertColorMatches c (color 0 0.99888 0.04725)

                    _ ->
                        Expect.fail "Whoopsie"
            )
        , test "shade_hit() with a transparent material"
            (\_ ->
                let
                    w =
                        defaultWorld
                            |> addObject floor
                            |> addObject ball

                    floor =
                        plane (Id 3)
                            |> Lib.Object.setTransform (translation 0 -1 0)
                            |> Lib.Object.setMaterial
                                { material
                                    | transparency = 0.5
                                    , refractiveIndex = 1.5
                                }

                    ball =
                        sphere (Id 4)
                            |> Lib.Object.setMaterial { material | color = color 1 0 0, ambient = 0.5 }
                            |> Lib.Object.setTransform (translation 0 -3.5 -0.5)

                    r =
                        Ray (point 0 0 -3) (vector 0 -(sqrt 2 / 2) (sqrt 2 / 2))

                    xs =
                        [ Intersection (sqrt 2) floor ]

                    comps =
                        prepareComputations r xs (Intersection (sqrt 2) floor)

                    c =
                        shadeHit w 5 comps
                in
                assertColorMatches c (color 0.93642 0.68642 0.68642)
            )
        , test "shade_hit() with a reflective, transparent material"
            (\_ ->
                let
                    w =
                        defaultWorld
                            |> addObject floor
                            |> addObject ball

                    r =
                        Ray (point 0 0 -3) (vector 0 -(sqrt 2 / 2) (sqrt 2 / 2))

                    floor =
                        plane (Id 10)
                            |> Lib.Object.setTransform (translation 0 -1 0)
                            |> Lib.Object.setMaterial
                                { material
                                    | reflective = 0.5
                                    , transparency = 0.5
                                    , refractiveIndex = 1.5
                                }

                    ball =
                        sphere (Id 11)
                            |> Lib.Object.setTransform (translation 0 -3.5 -0.5)
                            |> Lib.Object.setMaterial { material | color = color 1 0 0, ambient = 0.5 }

                    xs =
                        [ Intersection (sqrt 2) floor ]

                    comps =
                        prepareComputations r xs (Intersection (sqrt 2) floor)

                    col =
                        shadeHit w 5 comps
                in
                assertColorMatches col (color 0.93391 0.69643 0.69243)
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
