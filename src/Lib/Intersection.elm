module Lib.Intersection exposing (..)

import Lib.Matrix exposing (invert)
import Lib.Object exposing (Object, ObjectType(..), normalAt)
import Lib.Ray exposing (Ray, position, transform)
import Lib.Tuple exposing (Tuple, dot, point, subtract)
import List.Extra as List


type alias Intersection =
    { t : Float, object : Object }


intersections : Ray -> Object -> List Intersection
intersections r obj =
    let
        { origin, direction } =
            transform r (invert obj.transform)
    in
    case obj.type_ of
        Sphere ->
            let
                sphereToRay =
                    subtract origin (point 0 0 0)

                a =
                    dot direction direction

                b =
                    2 * dot direction sphereToRay

                c =
                    dot sphereToRay sphereToRay - 1

                discriminant =
                    b ^ 2 - 4 * a * c
            in
            if discriminant < 0 then
                []

            else
                let
                    t1 =
                        (-b - sqrt discriminant) / (2 * a)

                    t2 =
                        (-b + sqrt discriminant) / (2 * a)
                in
                [ { t = t1
                  , object = obj
                  }
                , { t = t2
                  , object = obj
                  }
                ]


hit : List Intersection -> Maybe Intersection
hit ints =
    ints
        |> List.sortBy .t
        |> List.filter (\{ t } -> t >= 0)
        |> List.head


type alias Computation =
    { t : Float
    , object : Object
    , point : Tuple
    , eyev : Tuple
    , normalv : Tuple
    , inside : Bool
    }


prepareComputations : Ray -> Intersection -> Computation
prepareComputations ray intersection =
    let
        pt =
            position ray intersection.t

        comps =
            { t = intersection.t
            , object = intersection.object
            , point = pt
            , eyev = Lib.Tuple.negate ray.direction
            , normalv = normalAt pt intersection.object
            , inside = False
            }
    in
    if dot comps.normalv comps.eyev < 0 then
        { comps | inside = True, normalv = Lib.Tuple.negate comps.normalv }

    else
        { comps | inside = False }
