module Main exposing (main)

import Browser
import Html
import Lib.Camera exposing (camera)
import Lib.Canvas exposing (Canvas, canvas, render)
import Lib.Color exposing (black, color, white)
import Lib.Intersection exposing (hit, intersections)
import Lib.Light exposing (pointLight)
import Lib.Material exposing (lighting, material)
import Lib.Matrix exposing (multiply, multiplyMany)
import Lib.Matrix.Transformation exposing (RotationAmount(..), rotationX, rotationY, scaling, translation, viewTransform)
import Lib.Object exposing (Id(..), normalAt, setMaterial, setTransform, sphere)
import Lib.Ray exposing (Ray, position)
import Lib.Tuple exposing (normalize, point, subtract, vector)
import Lib.World exposing (world)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { property : Int
    , property2 : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 "modelInitialValue2", Cmd.none )


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view _ =
    Html.div []
        [ scene2 |> render ]


myCanvas : Canvas
myCanvas =
    let
        rayOrigin =
            point 0 0 -5

        wallZ =
            10

        wallSize =
            7

        canvasPixels =
            200

        pixelSize =
            wallSize / canvasPixels

        half =
            wallSize / 2

        _ =
            color 1 0 0

        shape =
            sphere (Id 1)
                |> setMaterial { material | color = color 1 0.2 1 }

        lightPosition =
            point -10 10 -10

        lightColor =
            white

        light =
            pointLight lightPosition lightColor

        fn ( x, y ) =
            let
                worldY =
                    half - pixelSize * toFloat y

                worldX =
                    -half + pixelSize * toFloat x

                position =
                    point worldX worldY wallZ

                r =
                    Ray rayOrigin (normalize (subtract position rayOrigin))

                xs =
                    intersections r shape
            in
            case hit xs of
                Just h ->
                    let
                        pt =
                            Lib.Ray.position r h.t

                        normal =
                            normalAt pt h.object

                        eye =
                            Lib.Tuple.negate r.direction
                    in
                    lighting h.object.material pt eye normal light

                Nothing ->
                    black
    in
    canvas fn canvasPixels canvasPixels


scene2 : Canvas
scene2 =
    let
        floor =
            sphere (Id 1)
                |> setTransform (scaling 10 0.01 10)
                |> setMaterial { material | color = color 1 0.9 0.9, specular = 0 }

        leftWall =
            sphere (Id 2)
                |> setTransform
                    (multiplyMany
                        (scaling 10 0.01 10)
                        [ rotationX (Radians (pi / 2))
                        , rotationY (Radians (-pi / 4))
                        , translation 0 0 5
                        ]
                    )
                |> setMaterial floor.material

        rightWall =
            sphere (Id 3)
                |> setTransform
                    (multiplyMany
                        (scaling 10 0.01 10)
                        [ rotationX (Radians (pi / 2))
                        , rotationY (Radians (pi / 4))
                        , translation 0 0 5
                        ]
                    )
                |> setMaterial floor.material

        middle =
            sphere (Id 4)
                |> setTransform (translation -0.5 1 0.5)
                |> setMaterial
                    { material
                        | color = color 0.1 1 0.5
                        , diffuse = 0.7
                        , specular = 0.3
                    }

        right =
            sphere (Id 5)
                |> setTransform (multiply (translation 1.5 0.5 -0.5) (scaling 0.5 0.5 0.5))
                |> setMaterial
                    { material
                        | color = color 0.5 1 0.1
                        , diffuse = 0.7
                        , specular = 0.3
                    }

        left =
            sphere (Id 6)
                |> setTransform (multiply (translation -1.5 0.33 -0.75) (scaling 0.33 0.33 0.33))
                |> setMaterial
                    { material
                        | color = color 1 0.8 0.1
                        , diffuse = 0.7
                        , specular = 0.3
                    }

        w =
            { world
                | lights = [ pointLight (point -10 10 -10) white ]
                , objects = [ floor, leftWall, rightWall, middle, right, left ]
            }

        c =
            camera 100 50 (pi / 3)
                |> Lib.Camera.setTransform (viewTransform (point 0 1.5 -5) (point 0 1 0) (vector 0 1 0))
    in
    Lib.Camera.render c w
