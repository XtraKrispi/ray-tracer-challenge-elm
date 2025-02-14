module Lib.Camera exposing (..)

import Lib.Canvas exposing (Canvas, canvas)
import Lib.Matrix exposing (FourByFourMatrix, Matrix, identityMatrix, invert, multTuple)
import Lib.Ray exposing (Ray)
import Lib.Tuple exposing (normalize, point, subtract)
import Lib.World exposing (World, colorAt)


type alias Camera =
    { hsize : Int
    , vsize : Int
    , halfWidth : Float
    , halfHeight : Float
    , fieldOfView : Float
    , transform : Matrix FourByFourMatrix
    , pixelSize : Float
    }


camera : Int -> Int -> Float -> Camera
camera hsize vsize fieldOfView =
    let
        halfView =
            tan (fieldOfView / 2)

        aspect =
            toFloat hsize / toFloat vsize

        ( halfWidth, halfHeight ) =
            if aspect >= 1 then
                ( halfView, halfView / aspect )

            else
                ( halfView * aspect, halfView )
    in
    { hsize = hsize
    , vsize = vsize
    , halfWidth = halfWidth
    , halfHeight = halfHeight
    , fieldOfView = fieldOfView
    , transform = identityMatrix
    , pixelSize = (halfWidth * 2) / toFloat hsize
    }


setTransform : Matrix FourByFourMatrix -> Camera -> Camera
setTransform t c =
    { c | transform = t }


rayForPixel : Camera -> Float -> Float -> Ray
rayForPixel c px py =
    let
        -- The offset from the edge of the canvas to the pixel's center
        xOffset =
            (px + 0.5) * c.pixelSize

        yOffset =
            (py + 0.5) * c.pixelSize

        -- The untransformed coordinates of the pixel in world space.
        -- (Remember that the camera looks towards -z, so +x is to the left)
        worldX =
            c.halfWidth - xOffset

        worldY =
            c.halfHeight - yOffset

        -- Using the camera matrix, transform the canvas point and the origin,
        -- and then compute the ray's direction vector.
        -- (remember that the canvas is at z=-1)
        pixel =
            multTuple (invert c.transform) (point worldX worldY -1)

        origin =
            multTuple (invert c.transform) (point 0 0 0)

        direction =
            normalize (subtract pixel origin)
    in
    Ray origin direction


render : Camera -> World -> Canvas
render c w =
    canvas
        (\( x, y ) ->
            rayForPixel c (toFloat x) (toFloat y)
                |> colorAt w 5
        )
        c.hsize
        c.vsize
