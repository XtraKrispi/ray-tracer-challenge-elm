module Lib.Lighting exposing (..)

import Lib.Color exposing (Color, add, black, multiply, product)
import Lib.Light exposing (Light)
import Lib.Material exposing (Material)
import Lib.Object exposing (Object, patternAtShape)
import Lib.Tuple exposing (Tuple, dot, normalize, reflect, subtract)


lighting : Material -> Object -> Tuple -> Tuple -> Tuple -> Bool -> Light -> Color
lighting m object point eyev normalv inShadow light =
    let
        color =
            case m.pattern of
                Just p ->
                    patternAtShape point p object

                Nothing ->
                    m.color

        --combine the surface color with the light's color/intensity
        effectiveColor =
            product color light.intensity

        -- Find the direction to the light source
        lightv =
            normalize (subtract light.position point)

        -- compute the ambient contribution
        ambient =
            multiply effectiveColor m.ambient

        -- lightDotNormal represents the cosine of the angle between the
        -- light vector and the normal vector. A negative number means the
        -- light is on the other side of the surface.
        lightDotNormal =
            dot lightv normalv

        ( diffuse, specular ) =
            if lightDotNormal < 0 || inShadow then
                ( black, black )

            else
                -- compute the diffuse contribution
                let
                    d =
                        multiply (multiply effectiveColor m.diffuse) lightDotNormal

                    -- reflectDotEye represents the cosing of the angle between the
                    -- reflection vector and the eye vecor. A negative number means the
                    -- light reflects away from the eye.
                    reflectv =
                        reflect (Lib.Tuple.negate lightv) normalv

                    reflectDotEye =
                        dot reflectv eyev

                    s =
                        if reflectDotEye <= 0 then
                            black

                        else
                            -- compute the specular contribution
                            let
                                factor =
                                    reflectDotEye ^ m.shininess
                            in
                            multiply (multiply light.intensity m.specular) factor
                in
                ( d, s )
    in
    add ambient (add diffuse specular)
