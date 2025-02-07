module Lib.Object exposing (..)

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
import Lib.Tuple
    exposing
        ( Tuple
        , normalize
        , point
        , subtract
        )


type Id
    = Id Int


type ObjectType
    = Sphere


type alias Object =
    { id : Id
    , transform : Matrix FourByFourMatrix
    , type_ : ObjectType
    , material : Material
    }


sphere : Id -> Object
sphere id =
    { id = id
    , transform = identityMatrix
    , type_ = Sphere
    , material = material
    }


setTransform : Matrix FourByFourMatrix -> Object -> Object
setTransform m obj =
    { obj | transform = m }


setMaterial : Material -> Object -> Object
setMaterial m obj =
    { obj | material = m }


normalAt : Tuple -> Object -> Tuple
normalAt worldPoint obj =
    let
        objectPoint =
            multTuple (invert obj.transform) worldPoint

        objectNormal =
            subtract objectPoint (point 0 0 0)

        worldNormal =
            multTuple (transpose (invert obj.transform)) objectNormal
    in
    normalize { worldNormal | w = 0 }
