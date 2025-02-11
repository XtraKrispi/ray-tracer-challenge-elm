module Materials exposing (..)

import Expect
import Lib.Color exposing (Color, black, color, white)
import Lib.Light exposing (pointLight)
import Lib.Lighting exposing (lighting)
import Lib.Material exposing (material)
import Lib.Object exposing (Id(..), sphere)
import Lib.Pattern exposing (stripePattern)
import Lib.Tuple exposing (point, vector)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Material Tests"
        [ test "The default material"
            (\_ ->
                let
                    m =
                        material
                in
                Expect.all
                    [ \_ -> Expect.equal m.color (color 1 1 1)
                    , \_ -> Expect.within (Expect.Absolute 0.00001) m.ambient 0.1
                    , \_ -> Expect.within (Expect.Absolute 0.00001) m.diffuse 0.9
                    , \_ -> Expect.within (Expect.Absolute 0.00001) m.specular 0.9
                    , \_ -> Expect.within (Expect.Absolute 0.00001) m.shininess 200
                    ]
                    ()
            )
        , test "Lighting with the eye between the light and the surface"
            (\_ ->
                let
                    m =
                        material

                    position =
                        point 0 0 0

                    eyev =
                        vector 0 0 -1

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 0 -10) (color 1 1 1)
                in
                assertColorEqual (lighting m (sphere (Id 1)) position eyev normalv False light) (color 1.9 1.9 1.9)
            )
        , test "Lighting with the eye between light and surface, eye offset 45°"
            (\_ ->
                let
                    m =
                        material

                    position =
                        point 0 0 0

                    eyev =
                        vector 0 (sqrt 2 / 2) -(sqrt 2 / 2)

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 0 -10) (color 1 1 1)
                in
                assertColorEqual (lighting m (sphere (Id 1)) position eyev normalv False light) (color 1 1 1)
            )
        , test "Lighting with eye opposite surface, light offset 45°"
            (\_ ->
                let
                    m =
                        material

                    position =
                        point 0 0 0

                    eyev =
                        vector 0 0 -1

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 10 -10) (color 1 1 1)
                in
                assertColorEqual (lighting m (sphere (Id 1)) position eyev normalv False light) (color 0.7364 0.7364 0.7364)
            )
        , test "Lighting with eye in the path of the reflection vector"
            (\_ ->
                let
                    m =
                        material

                    position =
                        point 0 0 0

                    eyev =
                        vector 0 -(sqrt 2 / 2) -(sqrt 2 / 2)

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 10 -10) (color 1 1 1)
                in
                assertColorEqual (lighting m (sphere (Id 1)) position eyev normalv False light) (color 1.6364 1.6364 1.6364)
            )
        , test "Lighting with the light behind the surface"
            (\_ ->
                let
                    m =
                        material

                    position =
                        point 0 0 0

                    eyev =
                        vector 0 0 -1

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 0 10) (color 1 1 1)
                in
                assertColorEqual (lighting m (sphere (Id 1)) position eyev normalv False light) (color 0.1 0.1 0.1)
            )
        , test "Lighting with the surface in shadow"
            (\_ ->
                let
                    m =
                        material

                    position =
                        point 0 0 0

                    eyev =
                        vector 0 0 -1

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 0 -10) white

                    inShadow =
                        True

                    result =
                        lighting m (sphere (Id 1)) position eyev normalv inShadow light
                in
                assertColorEqual result (color 0.1 0.1 0.1)
            )
        , test "Lighting with a pattern applied"
            (\_ ->
                let
                    m =
                        { material
                            | ambient = 1
                            , diffuse = 0
                            , specular = 0
                            , pattern = Just (stripePattern white black)
                        }

                    eyev =
                        vector 0 0 -1

                    normalv =
                        vector 0 0 -1

                    light =
                        pointLight (point 0 0 -10) white

                    c1 =
                        lighting m (sphere (Id 1)) (point 0.9 0 0) eyev normalv False light

                    c2 =
                        lighting m (sphere (Id 1)) (point 1.1 0 0) eyev normalv False light
                in
                Expect.all
                    [ \_ -> assertColorEqual c1 white
                    , \_ -> assertColorEqual c2 black
                    ]
                    ()
            )
        ]


assertColorEqual : Color -> Color -> Expect.Expectation
assertColorEqual col1 col2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute 0.00001) col1.red col2.red
        , \_ -> Expect.within (Expect.Absolute 0.00001) col1.green col2.green
        , \_ -> Expect.within (Expect.Absolute 0.00001) col1.blue col2.blue
        ]
        ()
