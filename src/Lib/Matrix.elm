module Lib.Matrix exposing
    ( Col(..)
    , FourByFourMatrix
    , FourColumnRow(..)
    , Matrix
    , Row(..)
    , ThreeByThreeMatrix
    , ThreeColumnRow(..)
    , TwoByTwoMatrix
    , TwoColumnRow(..)
    , at
    , cofactor
    , determinant
    , equal
    , fourByFour
    , identityMatrix
    , invert
    , isInvertible
    , minor
    , multTuple
    , multiply
    , multiplyMany
    , submatrix
    , threeByThree
    , transpose
    , twoByTwo
    )

import Lib
import Lib.Tuple exposing (Tuple)
import List.Extra


type FourColumnRow
    = FourColumnRow Float Float Float Float


type ThreeColumnRow
    = ThreeColumnRow Float Float Float


type TwoColumnRow
    = TwoColumnRow Float Float


type FourByFourMatrix
    = FourByFourMatrix


type ThreeByThreeMatrix
    = ThreeByThreeMatrix


type TwoByTwoMatrix
    = TwoByTwoMatrix


type Matrix a
    = FourByFour FourColumnRow FourColumnRow FourColumnRow FourColumnRow
    | TwoByTwo TwoColumnRow TwoColumnRow
    | ThreeByThree ThreeColumnRow ThreeColumnRow ThreeColumnRow


equal : Matrix a -> Matrix a -> Bool
equal m1 m2 =
    case ( m1, m2 ) of
        ( TwoByTwo (TwoColumnRow r0c0 r0c1) (TwoColumnRow r1c0 r1c1), TwoByTwo (TwoColumnRow r0c0_ r0c1_) (TwoColumnRow r1c0_ r1c1_) ) ->
            Lib.equal r0c0 r0c0_
                && Lib.equal r0c1 r0c1_
                && Lib.equal r1c0 r1c0_
                && Lib.equal r1c1 r1c1_

        ( ThreeByThree (ThreeColumnRow r0c0 r0c1 r0c2) (ThreeColumnRow r1c0 r1c1 r1c2) (ThreeColumnRow r2c0 r2c1 r2c2), ThreeByThree (ThreeColumnRow r0c0_ r0c1_ r0c2_) (ThreeColumnRow r1c0_ r1c1_ r1c2_) (ThreeColumnRow r2c0_ r2c1_ r2c2_) ) ->
            Lib.equal r0c0 r0c0_
                && Lib.equal r0c1 r0c1_
                && Lib.equal r0c2 r0c2_
                && Lib.equal r1c0 r1c0_
                && Lib.equal r1c1 r1c1_
                && Lib.equal r1c2 r1c2_
                && Lib.equal r2c0 r2c0_
                && Lib.equal r2c1 r2c1_
                && Lib.equal r2c2 r2c2_

        ( FourByFour (FourColumnRow r0c0 r0c1 r0c2 r0c3) (FourColumnRow r1c0 r1c1 r1c2 r1c3) (FourColumnRow r2c0 r2c1 r2c2 r2c3) (FourColumnRow r3c0 r3c1 r3c2 r3c3), FourByFour (FourColumnRow r0c0_ r0c1_ r0c2_ r0c3_) (FourColumnRow r1c0_ r1c1_ r1c2_ r1c3_) (FourColumnRow r2c0_ r2c1_ r2c2_ r2c3_) (FourColumnRow r3c0_ r3c1_ r3c2_ r3c3_) ) ->
            Lib.equal r0c0 r0c0_
                && Lib.equal r0c1 r0c1_
                && Lib.equal r0c2 r0c2_
                && Lib.equal r0c3 r0c3_
                && Lib.equal r1c0 r1c0_
                && Lib.equal r1c1 r1c1_
                && Lib.equal r1c2 r1c2_
                && Lib.equal r1c3 r1c3_
                && Lib.equal r2c0 r2c0_
                && Lib.equal r2c1 r2c1_
                && Lib.equal r2c2 r2c2_
                && Lib.equal r2c3 r2c3_
                && Lib.equal r3c0 r3c0_
                && Lib.equal r3c1 r3c1_
                && Lib.equal r3c2 r3c2_
                && Lib.equal r3c3 r3c3_

        _ ->
            False


copy : Matrix a -> Matrix b
copy m =
    case m of
        FourByFour r1 r2 r3 r4 ->
            FourByFour r1 r2 r3 r4

        ThreeByThree r1 r2 r3 ->
            ThreeByThree r1 r2 r3

        TwoByTwo r1 r2 ->
            TwoByTwo r1 r2


fourByFour : FourColumnRow -> FourColumnRow -> FourColumnRow -> FourColumnRow -> Matrix FourByFourMatrix
fourByFour a b c d =
    FourByFour a b c d


twoByTwo : TwoColumnRow -> TwoColumnRow -> Matrix TwoByTwoMatrix
twoByTwo a b =
    TwoByTwo a b


threeByThree : ThreeColumnRow -> ThreeColumnRow -> ThreeColumnRow -> Matrix ThreeByThreeMatrix
threeByThree a b c =
    ThreeByThree a b c


type Row
    = Row Int


type Col
    = Col Int


toList : Matrix a -> List (List Float)
toList matrix =
    case matrix of
        FourByFour (FourColumnRow a b c d) (FourColumnRow e f g h) (FourColumnRow i j k l) (FourColumnRow m n o p) ->
            [ [ a, b, c, d ]
            , [ e, f, g, h ]
            , [ i, j, k, l ]
            , [ m, n, o, p ]
            ]

        ThreeByThree (ThreeColumnRow a b c) (ThreeColumnRow d e f) (ThreeColumnRow g h i) ->
            [ [ a, b, c ]
            , [ d, e, f ]
            , [ g, h, i ]
            ]

        TwoByTwo (TwoColumnRow a b) (TwoColumnRow c d) ->
            [ [ a, b ]
            , [ c, d ]
            ]


at : ( Row, Col ) -> Matrix a -> Maybe Float
at ( Row row, Col col ) matrix =
    matrix
        |> toList
        |> List.Extra.getAt row
        |> Maybe.andThen (List.Extra.getAt col)


multiply : Matrix a -> Matrix a -> Matrix a
multiply m1 m2 =
    case ( m1, m2 ) of
        ( FourByFour (FourColumnRow a00 a01 a02 a03) (FourColumnRow a10 a11 a12 a13) (FourColumnRow a20 a21 a22 a23) (FourColumnRow a30 a31 a32 a33), FourByFour (FourColumnRow b00 b01 b02 b03) (FourColumnRow b10 b11 b12 b13) (FourColumnRow b20 b21 b22 b23) (FourColumnRow b30 b31 b32 b33) ) ->
            FourByFour
                (FourColumnRow
                    (a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30)
                    (a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31)
                    (a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32)
                    (a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33)
                )
                (FourColumnRow
                    (a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30)
                    (a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31)
                    (a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32)
                    (a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33)
                )
                (FourColumnRow
                    (a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30)
                    (a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31)
                    (a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32)
                    (a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33)
                )
                (FourColumnRow
                    (a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30)
                    (a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31)
                    (a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32)
                    (a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33)
                )

        ( ThreeByThree (ThreeColumnRow a00 a01 a02) (ThreeColumnRow a10 a11 a12) (ThreeColumnRow a20 a21 a22), ThreeByThree (ThreeColumnRow b00 b01 b02) (ThreeColumnRow b10 b11 b12) (ThreeColumnRow b20 b21 b22) ) ->
            ThreeByThree
                (ThreeColumnRow
                    (a00 * b00 + a01 * b10 + a02 * b20)
                    (a00 * b01 + a01 * b11 + a02 * b21)
                    (a00 * b02 + a01 * b12 + a02 * b22)
                )
                (ThreeColumnRow
                    (a10 * b00 + a11 * b10 + a12 * b20)
                    (a10 * b01 + a11 * b11 + a12 * b21)
                    (a10 * b02 + a11 * b12 + a12 * b22)
                )
                (ThreeColumnRow
                    (a20 * b00 + a21 * b10 + a22 * b20)
                    (a20 * b01 + a21 * b11 + a22 * b21)
                    (a20 * b02 + a21 * b12 + a22 * b22)
                )

        ( TwoByTwo (TwoColumnRow a00 a01) (TwoColumnRow a10 a11), TwoByTwo (TwoColumnRow b00 b01) (TwoColumnRow b10 b11) ) ->
            TwoByTwo
                (TwoColumnRow
                    (a00 * b00 + a01 * b10)
                    (a00 * b01 + a01 * b11)
                )
                (TwoColumnRow
                    (a10 * b00 + a11 * b10)
                    (a10 * b01 + a11 * b11)
                )

        _ ->
            m1


multiplyMany : Matrix a -> List (Matrix a) -> Matrix a
multiplyMany head xs =
    List.foldl multiply head xs


multTuple : Matrix FourByFourMatrix -> Tuple -> Tuple
multTuple m t =
    case m of
        FourByFour (FourColumnRow a00 a01 a02 a03) (FourColumnRow a10 a11 a12 a13) (FourColumnRow a20 a21 a22 a23) (FourColumnRow a30 a31 a32 a33) ->
            Tuple
                (a00 * t.x + a01 * t.y + a02 * t.z + a03 * t.w)
                (a10 * t.x + a11 * t.y + a12 * t.z + a13 * t.w)
                (a20 * t.x + a21 * t.y + a22 * t.z + a23 * t.w)
                (a30 * t.x + a31 * t.y + a32 * t.z + a33 * t.w)

        _ ->
            t


identityMatrix : Matrix FourByFourMatrix
identityMatrix =
    FourByFour
        (FourColumnRow 1 0 0 0)
        (FourColumnRow 0 1 0 0)
        (FourColumnRow 0 0 1 0)
        (FourColumnRow 0 0 0 1)


transpose : Matrix a -> Matrix a
transpose m =
    case m of
        FourByFour (FourColumnRow a00 a01 a02 a03) (FourColumnRow a10 a11 a12 a13) (FourColumnRow a20 a21 a22 a23) (FourColumnRow a30 a31 a32 a33) ->
            FourByFour
                (FourColumnRow a00 a10 a20 a30)
                (FourColumnRow a01 a11 a21 a31)
                (FourColumnRow a02 a12 a22 a32)
                (FourColumnRow a03 a13 a23 a33)

        ThreeByThree (ThreeColumnRow a00 a01 a02) (ThreeColumnRow a10 a11 a12) (ThreeColumnRow a20 a21 a22) ->
            ThreeByThree
                (ThreeColumnRow a00 a10 a20)
                (ThreeColumnRow a01 a11 a21)
                (ThreeColumnRow a02 a12 a22)

        TwoByTwo (TwoColumnRow a00 a01) (TwoColumnRow a10 a11) ->
            TwoByTwo
                (TwoColumnRow a00 a10)
                (TwoColumnRow a01 a11)


determinant : Matrix a -> Float
determinant m =
    case m of
        TwoByTwo (TwoColumnRow a b) (TwoColumnRow c d) ->
            a * d - b * c

        ThreeByThree (ThreeColumnRow a b c) _ _ ->
            a * cofactor ( Row 0, Col 0 ) m + b * cofactor ( Row 0, Col 1 ) m + c * cofactor ( Row 0, Col 2 ) m

        FourByFour (FourColumnRow a b c d) _ _ _ ->
            a * cofactor ( Row 0, Col 0 ) m + b * cofactor ( Row 0, Col 1 ) m + c * cofactor ( Row 0, Col 2 ) m + d * cofactor ( Row 0, Col 3 ) m


removeFromFourColumn : Int -> FourColumnRow -> ThreeColumnRow
removeFromFourColumn col (FourColumnRow a b c d) =
    case col of
        0 ->
            ThreeColumnRow b c d

        1 ->
            ThreeColumnRow a c d

        2 ->
            ThreeColumnRow a b d

        3 ->
            ThreeColumnRow a b c

        _ ->
            ThreeColumnRow a b c


removeFromThreeColumn : Int -> ThreeColumnRow -> TwoColumnRow
removeFromThreeColumn col (ThreeColumnRow a b c) =
    case col of
        0 ->
            TwoColumnRow b c

        1 ->
            TwoColumnRow a c

        2 ->
            TwoColumnRow a b

        _ ->
            TwoColumnRow a b


submatrix : ( Row, Col ) -> Matrix a -> Matrix b
submatrix ( Row row, Col col ) m =
    case m of
        FourByFour r1 r2 r3 r4 ->
            case row of
                0 ->
                    ThreeByThree (removeFromFourColumn col r2) (removeFromFourColumn col r3) (removeFromFourColumn col r4)

                1 ->
                    ThreeByThree (removeFromFourColumn col r1) (removeFromFourColumn col r3) (removeFromFourColumn col r4)

                2 ->
                    ThreeByThree (removeFromFourColumn col r1) (removeFromFourColumn col r2) (removeFromFourColumn col r4)

                3 ->
                    ThreeByThree (removeFromFourColumn col r1) (removeFromFourColumn col r2) (removeFromFourColumn col r3)

                _ ->
                    copy m

        ThreeByThree r1 r2 r3 ->
            case row of
                0 ->
                    TwoByTwo (removeFromThreeColumn col r2) (removeFromThreeColumn col r3)

                1 ->
                    TwoByTwo (removeFromThreeColumn col r1) (removeFromThreeColumn col r3)

                2 ->
                    TwoByTwo (removeFromThreeColumn col r1) (removeFromThreeColumn col r2)

                _ ->
                    copy m

        TwoByTwo _ _ ->
            copy m


minor : ( Row, Col ) -> Matrix a -> Float
minor coord m =
    let
        b =
            submatrix coord m
    in
    determinant b


cofactor : ( Row, Col ) -> Matrix a -> Float
cofactor (( Row row, Col col ) as coord) m =
    minor coord m
        * (if modBy 2 (row + col) == 0 then
            1

           else
            -1
          )


isInvertible : Matrix a -> Bool
isInvertible m =
    determinant m /= 0


invert : Matrix a -> Matrix a
invert m =
    let
        det =
            determinant m
    in
    case m of
        TwoByTwo _ _ ->
            TwoByTwo
                (TwoColumnRow
                    (cofactor ( Row 0, Col 0 ) m / det)
                    (cofactor ( Row 1, Col 0 ) m / det)
                )
                (TwoColumnRow
                    (cofactor ( Row 0, Col 1 ) m / det)
                    (cofactor ( Row 1, Col 1 ) m / det)
                )

        ThreeByThree _ _ _ ->
            ThreeByThree
                (ThreeColumnRow
                    (cofactor ( Row 0, Col 0 ) m / det)
                    (cofactor ( Row 1, Col 0 ) m / det)
                    (cofactor ( Row 2, Col 0 ) m / det)
                )
                (ThreeColumnRow
                    (cofactor ( Row 0, Col 1 ) m / det)
                    (cofactor ( Row 1, Col 1 ) m / det)
                    (cofactor ( Row 2, Col 1 ) m / det)
                )
                (ThreeColumnRow
                    (cofactor ( Row 0, Col 2 ) m / det)
                    (cofactor ( Row 1, Col 2 ) m / det)
                    (cofactor ( Row 2, Col 2 ) m / det)
                )

        FourByFour _ _ _ _ ->
            FourByFour
                (FourColumnRow
                    (cofactor ( Row 0, Col 0 ) m / det)
                    (cofactor ( Row 1, Col 0 ) m / det)
                    (cofactor ( Row 2, Col 0 ) m / det)
                    (cofactor ( Row 3, Col 0 ) m / det)
                )
                (FourColumnRow
                    (cofactor ( Row 0, Col 1 ) m / det)
                    (cofactor ( Row 1, Col 1 ) m / det)
                    (cofactor ( Row 2, Col 1 ) m / det)
                    (cofactor ( Row 3, Col 1 ) m / det)
                )
                (FourColumnRow
                    (cofactor ( Row 0, Col 2 ) m / det)
                    (cofactor ( Row 1, Col 2 ) m / det)
                    (cofactor ( Row 2, Col 2 ) m / det)
                    (cofactor ( Row 3, Col 2 ) m / det)
                )
                (FourColumnRow
                    (cofactor ( Row 0, Col 3 ) m / det)
                    (cofactor ( Row 1, Col 3 ) m / det)
                    (cofactor ( Row 2, Col 3 ) m / det)
                    (cofactor ( Row 3, Col 3 ) m / det)
                )
