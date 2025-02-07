module Lib.World exposing (..)

import Lib.Color exposing (Color, black, color, white)
import Lib.Intersection exposing (Computation, Intersection, hit, intersections, prepareComputations)
import Lib.Light exposing (Light, pointLight)
import Lib.Material exposing (lighting, material)
import Lib.Matrix.Transformation exposing (scaling)
import Lib.Object exposing (Id(..), Object, setMaterial, setTransform, sphere)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (point)


type alias World =
    { lights : List Light
    , objects : List Object
    }


world : World
world =
    { lights = []
    , objects = []
    }


defaultWorld : World
defaultWorld =
    { lights = [ pointLight (point -10 10 -10) white ]
    , objects =
        [ sphere (Id 1)
            |> setMaterial
                { material
                    | color = color 0.8 1 0.6
                    , diffuse = 0.7
                    , specular = 0.2
                }
        , sphere (Id 1)
            |> setTransform (scaling 0.5 0.5 0.5)
        ]
    }


shadeHit : World -> Computation -> Color
shadeHit w comps =
    w.lights
        |> List.map (lighting comps.object.material comps.point comps.eyev comps.normalv)
        |> List.foldr Lib.Color.add black


intersectWorld : World -> Ray -> List Intersection
intersectWorld { objects } ray =
    objects
        |> List.map (intersections ray)
        |> List.concat
        |> List.sortBy .t


colorAt : World -> Ray -> Color
colorAt w r =
    intersectWorld w r
        |> hit
        |> Maybe.map (shadeHit w << prepareComputations r)
        |> Maybe.withDefault black
