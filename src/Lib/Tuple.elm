module Lib.Tuple exposing (..)

import Lib


type alias Tuple =
    { x : Float, y : Float, z : Float, w : Float }


equal : Tuple -> Tuple -> Bool
equal t1 t2 =
    Lib.equal t1.x t2.x
        && Lib.equal t1.y t2.y
        && Lib.equal t1.z t2.z
        && Lib.equal t1.w t2.w


point : Float -> Float -> Float -> Tuple
point x y z =
    Tuple x y z 1.0


vector : Float -> Float -> Float -> Tuple
vector x y z =
    Tuple x y z 0


add : Tuple -> Tuple -> Tuple
add t1 t2 =
    Tuple (t1.x + t2.x) (t1.y + t2.y) (t1.z + t2.z) (t1.w + t2.w)


subtract : Tuple -> Tuple -> Tuple
subtract t1 t2 =
    Tuple (t1.x - t2.x) (t1.y - t2.y) (t1.z - t2.z) (t1.w - t2.w)


negate : Tuple -> Tuple
negate t =
    subtract (Tuple 0 0 0 0) t


multiply : Tuple -> Float -> Tuple
multiply { x, y, z, w } s =
    Tuple (x * s) (y * s) (z * s) (w * s)


divide : Tuple -> Float -> Tuple
divide { x, y, z, w } s =
    Tuple (x / s) (y / s) (z / s) (w / s)


magnitude : Tuple -> Float
magnitude { x, y, z, w } =
    sqrt (x * x + y * y + z * z + w * w)


normalize : Tuple -> Tuple
normalize ({ x, y, z, w } as t) =
    let
        m =
            magnitude t
    in
    Tuple (x / m) (y / m) (z / m) (w / m)


dot : Tuple -> Tuple -> Float
dot t1 t2 =
    t1.x * t2.x + t1.y * t2.y + t1.z * t2.z + t1.w * t2.w


cross : Tuple -> Tuple -> Tuple
cross a b =
    vector (a.y * b.z - a.z * b.y) (a.z * b.x - a.x * b.z) (a.x * b.y - a.y * b.x)


reflect : Tuple -> Tuple -> Tuple
reflect i normal =
    subtract i (multiply (multiply normal 2) (dot i normal))
