module Lib.Object exposing (..)

import Lib.Color exposing (Color, black)
import Lib.Material exposing (Material, material)
import Lib.Matrix
    exposing
        ( FourByFourMatrix
        , Matrix
        , identityMatrix
        , invert
        , multTuple
        , transpose
        )
import Lib.Pattern exposing (Pattern, patternAt)
import Lib.Tuple
    exposing
        ( Tuple
        , normalize
        , point
        , subtract
        , vector
        )


type Id
    = Id Int


type Shape
    = Sphere
    | TestShape
    | Plane


type alias Object =
    { id : Id
    , transform : Matrix FourByFourMatrix
    , shape : Shape
    , material : Material
    }


sphere : Id -> Object
sphere id =
    { id = id
    , transform = identityMatrix
    , shape = Sphere
    , material = material
    }


glassSphere : Id -> Object
glassSphere id =
    { id = id
    , transform = identityMatrix
    , shape = Sphere
    , material = { material | transparency = 1, refractiveIndex = 1.5 }
    }


testShape : Id -> Object
testShape id =
    { id = id
    , transform = identityMatrix
    , shape = TestShape
    , material = material
    }


plane : Id -> Object
plane id =
    { id = id
    , transform = identityMatrix
    , shape = Plane
    , material = material
    }


setTransform : Matrix FourByFourMatrix -> Object -> Object
setTransform m obj =
    { obj | transform = m }


setMaterial : Material -> Object -> Object
setMaterial m obj =
    { obj | material = m }


localNormalAt : Tuple -> Object -> Tuple
localNormalAt localPoint obj =
    case obj.shape of
        TestShape ->
            vector localPoint.x localPoint.y localPoint.z

        Sphere ->
            subtract localPoint (point 0 0 0)

        Plane ->
            vector 0 1 0


normalAt : Tuple -> Object -> Tuple
normalAt worldPoint obj =
    let
        localPoint =
            multTuple (invert obj.transform) worldPoint

        localNormal =
            localNormalAt localPoint obj

        worldNormal =
            multTuple (transpose (invert obj.transform)) localNormal
    in
    normalize { worldNormal | w = 0 }


patternAtShape : Tuple -> Pattern -> Object -> Color
patternAtShape worldPoint pattern object =
    let
        objectPoint =
            multTuple (invert object.transform) worldPoint

        patternPoint =
            multTuple (invert pattern.transform) objectPoint
    in
    patternAt patternPoint pattern
