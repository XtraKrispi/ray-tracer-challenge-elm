module Lib.Intersection exposing (..)

import Lib exposing (epsilon)
import Lib.Matrix exposing (invert)
import Lib.Object exposing (Object, Shape(..), normalAt)
import Lib.Ray exposing (Ray, position, transform)
import Lib.Tuple exposing (Tuple, add, dot, multiply, point, reflect, subtract)
import List
import List.Extra as List


type alias Intersection =
    { t : Float, object : Object }


localIntersections : Ray -> Object -> List Intersection
localIntersections { origin, direction } obj =
    case obj.shape of
        TestShape ->
            []

        Plane ->
            if abs direction.y < epsilon then
                []

            else
                [ { t = -origin.y / direction.y, object = obj } ]

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

        Cube ->
            let
                ( xtmin, xtmax ) =
                    checkAxis origin.x direction.x

                ( ytmin, ytmax ) =
                    checkAxis origin.y direction.y

                ( ztmin, ztmax ) =
                    checkAxis origin.z direction.z

                tmin =
                    max (max xtmin ytmin) ztmin

                tmax =
                    min (min xtmax ytmax) ztmax
            in
            if tmin > tmax then
                []

            else
                [ Intersection tmin obj, Intersection tmax obj ]

        Cylinder ->
            []


signum : Float -> Float
signum f =
    if f < 0 then
        -1

    else
        1


checkAxis : Float -> Float -> ( Float, Float )
checkAxis origin direction =
    let
        tminNumerator =
            -1 - origin

        tmaxNumerator =
            1 - origin

        ( tmin, tmax ) =
            if abs direction >= epsilon then
                ( tminNumerator / direction, tmaxNumerator / direction )

            else
                ( signum tminNumerator * 2147483647, signum tmaxNumerator * 2147483647 )
    in
    if tmin > tmax then
        ( tmax, tmin )

    else
        ( tmin, tmax )


intersections : Ray -> Object -> List Intersection
intersections r obj =
    let
        localRay =
            transform r (invert obj.transform)
    in
    localIntersections localRay obj


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
    , overPoint : Tuple
    , inside : Bool
    , reflectv : Tuple
    , n1 : Float
    , n2 : Float
    , underPoint : Tuple
    }


prepareComputations : Ray -> List Intersection -> Intersection -> Computation
prepareComputations ray xs h =
    let
        pt =
            position ray h.t

        comps =
            { t = h.t
            , object = h.object
            , point = pt
            , eyev = Lib.Tuple.negate ray.direction
            , normalv = normalAt pt h.object
            , inside = False
            , overPoint = pt
            , reflectv = pt
            , n1 = 0
            , n2 = 0
            , underPoint = pt
            }

        results =
            if dot comps.normalv comps.eyev < 0 then
                { comps | inside = True, normalv = Lib.Tuple.negate comps.normalv }

            else
                { comps | inside = False }

        { n1, n2 } =
            go h xs ( [], { n1 = 0, n2 = 0 } )
    in
    { results
        | overPoint = add results.point (multiply results.normalv epsilon)
        , underPoint = subtract results.point (multiply results.normalv epsilon)
        , reflectv = reflect ray.direction results.normalv
        , n1 = n1
        , n2 = n2
    }


go :
    Intersection
    -> List Intersection
    -> ( List Object, { n1 : Float, n2 : Float } )
    -> { n1 : Float, n2 : Float }
go h xs ( containers, { n1, n2 } ) =
    case xs of
        [] ->
            { n1 = 1, n2 = 1 }

        i :: rest ->
            let
                isHit =
                    h == i

                compsN1 =
                    if isHit then
                        if List.isEmpty containers then
                            1

                        else
                            List.last containers
                                |> Maybe.map (\m -> m.material.refractiveIndex)
                                |> Maybe.withDefault 0

                    else
                        1

                newContainers =
                    if List.member i.object containers then
                        List.remove i.object containers

                    else
                        containers ++ [ i.object ]

                compsN2 =
                    if isHit then
                        if List.isEmpty newContainers then
                            1

                        else
                            List.last newContainers
                                |> Maybe.map (\m -> m.material.refractiveIndex)
                                |> Maybe.withDefault 0

                    else
                        1
            in
            if isHit then
                { n1 = compsN1, n2 = compsN2 }

            else
                go h rest ( newContainers, { n1 = compsN1, n2 = compsN2 } )


schlick : Computation -> Float
schlick comps =
    let
        cos =
            dot comps.eyev comps.normalv
    in
    if comps.n1 > comps.n2 then
        let
            n =
                comps.n1 / comps.n2

            sin2T =
                (n ^ 2) * (1 - (cos ^ 2))
        in
        if sin2T > 1 then
            1

        else
            let
                cosT =
                    sqrt (1 - sin2T)

                r0 =
                    ((comps.n1 - comps.n2) / (comps.n1 + comps.n2)) ^ 2
            in
            r0 + (1 - r0) * (1 - cosT) ^ 5

    else
        let
            r0 =
                ((comps.n1 - comps.n2) / (comps.n1 + comps.n2)) ^ 2
        in
        r0 + (1 - r0) * (1 - cos) ^ 5
