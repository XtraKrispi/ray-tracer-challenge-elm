module Lib.Canvas exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, style)
import Lib.Color exposing (Color)


type alias Canvas =
    { width : Int, height : Int, pixels : Dict ( Int, Int ) Color }


canvas : (( Int, Int ) -> Color) -> Int -> Int -> Canvas
canvas f width height =
    List.repeat width 0
        |> List.indexedMap
            (\x _ ->
                List.repeat height 0
                    |> List.indexedMap (\y _ -> ( ( x, y ), f ( x, y ) ))
            )
        |> List.concat
        |> Dict.fromList
        |> Canvas width height


writePixel : ( Int, Int ) -> Color -> Canvas -> Canvas
writePixel coord col c =
    { c | pixels = Dict.insert coord col c.pixels }


pixelAt : ( Int, Int ) -> Canvas -> Maybe Color
pixelAt coord c =
    c.pixels
        |> Dict.get coord


render : Canvas -> Html msg
render c =
    c.pixels
        |> Dict.toList
        |> List.sortBy (\( ( _, y ), _ ) -> y)
        |> List.map
            (\( ( x, y ), col ) ->
                div
                    [ attribute "data-x" (String.fromInt x)
                    , attribute "data-y" (String.fromInt y)
                    , style "background-color" ("rgb(" ++ String.fromFloat (col.red * 255) ++ "," ++ String.fromFloat (col.green * 255) ++ "," ++ String.fromFloat (col.blue * 255) ++ ")")
                    , style "width" "1px"
                    , style "height" "1px"
                    ]
                    []
            )
        |> div
            [ style "display" "inline-grid"
            , style "grid-template-columns" (List.repeat c.width "1px" |> String.join " ")
            ]
