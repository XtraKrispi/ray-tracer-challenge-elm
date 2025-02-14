module Lib.World exposing (..)

import Lib.Color exposing (Color, black, color, white)
import Lib.Intersection exposing (Computation, Intersection, hit, intersections, prepareComputations, schlick)
import Lib.Light exposing (Light, pointLight)
import Lib.Lighting exposing (lighting)
import Lib.Material exposing (material)
import Lib.Matrix.Transformation exposing (scaling)
import Lib.Object exposing (Id(..), Object, setMaterial, setTransform, sphere)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (Tuple, dot, magnitude, normalize, point, subtract)


type alias World =
    { lights : List Light
    , objects : List Object
    }


emptyWorld : World
emptyWorld =
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
        , sphere (Id 2)
            |> setTransform (scaling 0.5 0.5 0.5)
        ]
    }


mapObjects : (Object -> Object) -> World -> World
mapObjects fn w =
    { w | objects = w.objects |> List.map fn }


indexedMapObjects : (Int -> Object -> Object) -> World -> World
indexedMapObjects fn w =
    { w | objects = w.objects |> List.indexedMap fn }


shadeHit : World -> Int -> Computation -> Color
shadeHit w remaining comps =
    let
        shadowed =
            isShadowed w comps.overPoint

        surface =
            w.lights
                |> List.map (lighting comps.object.material comps.object comps.point comps.eyev comps.normalv shadowed)
                |> List.foldr Lib.Color.add black

        reflected =
            reflectedColor w remaining comps

        refracted =
            refractedColor w remaining comps
    in
    if comps.object.material.reflective > 0 && comps.object.material.transparency > 0 then
        let
            reflectance =
                schlick comps
        in
        Lib.Color.add
            (Lib.Color.add
                surface
                (Lib.Color.multiply reflected reflectance)
            )
            (Lib.Color.multiply refracted (1 - reflectance))

    else
        Lib.Color.add (Lib.Color.add surface reflected) refracted


intersectWorld : World -> Ray -> List Intersection
intersectWorld { objects } ray =
    objects
        |> List.map (intersections ray)
        |> List.concat
        |> List.sortBy .t


colorAt : World -> Int -> Ray -> Color
colorAt w remaining r =
    let
        intersections =
            intersectWorld w r
    in
    intersections
        |> hit
        |> Maybe.map (shadeHit w remaining << prepareComputations r intersections)
        |> Maybe.withDefault black


isShadowed : World -> Tuple -> Bool
isShadowed w p =
    let
        perLight light =
            let
                v =
                    subtract light.position p

                distance =
                    magnitude v

                direction =
                    normalize v

                r =
                    Ray p direction

                intersections =
                    intersectWorld w r

                h =
                    hit intersections
            in
            h |> Maybe.map (\h_ -> h_.t < distance) |> Maybe.withDefault False
    in
    w.lights
        |> List.any perLight


addLight : Light -> World -> World
addLight light w =
    { w | lights = w.lights ++ [ light ] }


addObject : Object -> World -> World
addObject obj w =
    { w | objects = w.objects ++ [ obj ] }


reflectedColor : World -> Int -> Computation -> Color
reflectedColor w remaining comps =
    if comps.object.material.reflective == 0 || remaining <= 0 then
        black

    else
        let
            reflectRay =
                Ray comps.overPoint comps.reflectv

            col =
                colorAt w (remaining - 1) reflectRay
        in
        Lib.Color.multiply col comps.object.material.reflective


refractedColor : World -> Int -> Computation -> Color
refractedColor w remaining comps =
    if comps.object.material.transparency == 0 || remaining <= 0 then
        black

    else
        let
            nRatio =
                comps.n1 / comps.n2

            cosI =
                dot comps.eyev comps.normalv

            sin2T =
                (nRatio ^ 2) * (1 - (cosI ^ 2))
        in
        if sin2T > 1 then
            black

        else
            let
                cosT =
                    sqrt (1 - sin2T)

                direction =
                    Lib.Tuple.subtract
                        (Lib.Tuple.multiply comps.normalv
                            (nRatio * cosI - cosT)
                        )
                        (Lib.Tuple.multiply comps.eyev nRatio)

                refractRay =
                    Ray comps.underPoint direction
            in
            Lib.Color.multiply (colorAt w (remaining - 1) refractRay) comps.object.material.transparency
