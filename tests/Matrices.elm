module Matrices exposing (..)

import Expect
import Lib.Matrix
    exposing
        ( Col(..)
        , FourColumnRow(..)
        , Matrix(..)
        , Row(..)
        , ThreeColumnRow(..)
        , TwoColumnRow(..)
        , at
        , cofactor
        , determinant
        , fourByFour
        , identityMatrix
        , invert
        , isInvertible
        , minor
        , multTuple
        , multiply
        , submatrix
        , threeByThree
        , transpose
        , twoByTwo
        )
import Lib.Tuple exposing (Tuple)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Matrix Operations"
        [ test "Constructing and inspecting a 4x4 matrix"
            (\_ ->
                let
                    m =
                        fourByFour (FourColumnRow 1 2 3 4) (FourColumnRow 5.5 6.5 7.5 8.5) (FourColumnRow 9 10 11 12) (FourColumnRow 13.5 14.5 15.5 16.6)
                in
                Expect.all
                    [ checkNumberAt ( 0, 0 ) 1
                    , checkNumberAt ( 0, 3 ) 4
                    , checkNumberAt ( 1, 0 ) 5.5
                    , checkNumberAt ( 1, 2 ) 7.5
                    , checkNumberAt ( 2, 2 ) 11
                    , checkNumberAt ( 3, 0 ) 13.5
                    , checkNumberAt ( 3, 2 ) 15.5
                    ]
                    m
            )
        , test "A 2x2 matrix ought to be representable"
            (\_ ->
                let
                    m =
                        twoByTwo (TwoColumnRow -3 5) (TwoColumnRow 1 -2)
                in
                Expect.all
                    [ checkNumberAt ( 0, 0 ) -3
                    , checkNumberAt ( 0, 1 ) 5
                    , checkNumberAt ( 1, 0 ) 1
                    , checkNumberAt ( 1, 1 ) -2
                    ]
                    m
            )
        , test "A 3x3 matrix ought to be representable"
            (\_ ->
                let
                    m =
                        threeByThree (ThreeColumnRow -3 5 0) (ThreeColumnRow 1 -2 -7) (ThreeColumnRow 0 1 1)
                in
                Expect.all
                    [ checkNumberAt ( 0, 0 ) -3
                    , checkNumberAt ( 1, 1 ) -2
                    , checkNumberAt ( 2, 2 ) 1
                    ]
                    m
            )
        , test "Multiplying two matrices"
            (\_ ->
                let
                    m1 =
                        fourByFour
                            (FourColumnRow 1 2 3 4)
                            (FourColumnRow 5 6 7 8)
                            (FourColumnRow 9 8 7 6)
                            (FourColumnRow 5 4 3 2)

                    m2 =
                        fourByFour
                            (FourColumnRow -2 1 2 3)
                            (FourColumnRow 3 2 1 -1)
                            (FourColumnRow 4 3 6 5)
                            (FourColumnRow 1 2 7 8)

                    result =
                        multiply m1 m2

                    expectedResult =
                        fourByFour
                            (FourColumnRow 20 22 50 48)
                            (FourColumnRow 44 54 114 108)
                            (FourColumnRow 40 58 110 102)
                            (FourColumnRow 16 26 46 42)
                in
                Expect.equal result expectedResult
            )
        , test "A matrix multiplied by a tuple"
            (\_ ->
                let
                    m =
                        fourByFour
                            (FourColumnRow 1 2 3 4)
                            (FourColumnRow 2 4 4 2)
                            (FourColumnRow 8 6 4 1)
                            (FourColumnRow 0 0 0 1)

                    b =
                        Tuple 1 2 3 1

                    result =
                        multTuple m b

                    expectedResult =
                        Tuple 18 24 33 1
                in
                Expect.equal result expectedResult
            )
        , test "Multiplying a matrix by the identity matrix"
            (\_ ->
                let
                    m =
                        fourByFour
                            (FourColumnRow 0 1 2 4)
                            (FourColumnRow 1 2 4 8)
                            (FourColumnRow 2 4 8 16)
                            (FourColumnRow 4 8 16 32)

                    result =
                        multiply m identityMatrix
                in
                Expect.equal result m
            )
        , test "Transposing a matrix"
            (\_ ->
                let
                    m =
                        fourByFour
                            (FourColumnRow 0 9 3 0)
                            (FourColumnRow 9 8 0 8)
                            (FourColumnRow 1 8 5 3)
                            (FourColumnRow 0 0 5 8)

                    result =
                        transpose m

                    expectedResult =
                        fourByFour
                            (FourColumnRow 0 9 1 0)
                            (FourColumnRow 9 8 8 0)
                            (FourColumnRow 3 0 5 5)
                            (FourColumnRow 0 8 3 8)
                in
                Expect.equal result expectedResult
            )
        , test "Transposing the identity matrix"
            (\_ ->
                let
                    result =
                        transpose identityMatrix
                in
                Expect.equal result identityMatrix
            )
        , test "Calculating the determinant of a 2x2 matrix"
            (\_ ->
                let
                    a =
                        twoByTwo (TwoColumnRow 1 5) (TwoColumnRow -3 2)

                    result =
                        determinant a
                in
                Expect.equal result 17
            )
        , test "A submatrix of a 3x3 matrix is a 2x2 matrix"
            (\_ ->
                let
                    a =
                        threeByThree (ThreeColumnRow 1 5 0) (ThreeColumnRow -3 2 7) (ThreeColumnRow 0 6 -3)

                    result =
                        submatrix ( Row 0, Col 2 ) a

                    expectedResult =
                        twoByTwo (TwoColumnRow -3 2) (TwoColumnRow 0 6)
                in
                Expect.equal result expectedResult
            )
        , test "A submatrix of a 4x4 matrix is a 3x3 matrix"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow -6 1 1 6)
                            (FourColumnRow -8 5 8 6)
                            (FourColumnRow -1 0 8 2)
                            (FourColumnRow -7 1 -1 1)

                    result =
                        submatrix ( Row 2, Col 1 ) a

                    expectedResult =
                        threeByThree
                            (ThreeColumnRow -6 1 6)
                            (ThreeColumnRow -8 8 6)
                            (ThreeColumnRow -7 -1 1)
                in
                Expect.equal result expectedResult
            )
        , test "Calculating a minor of a 3x3 matrix"
            (\_ ->
                let
                    a =
                        threeByThree
                            (ThreeColumnRow 3 5 0)
                            (ThreeColumnRow 2 -1 -7)
                            (ThreeColumnRow 6 -1 5)
                in
                Expect.all
                    [ \m -> Expect.equal (determinant (submatrix ( Row 1, Col 0 ) m)) 25
                    , \m -> Expect.equal (minor ( Row 1, Col 0 ) m) 25
                    ]
                    a
            )
        , test "Calculating a cofactor of a 3x3 matrix"
            (\_ ->
                let
                    a =
                        threeByThree
                            (ThreeColumnRow 3 5 0)
                            (ThreeColumnRow 2 -1 -7)
                            (ThreeColumnRow 6 -1 5)
                in
                Expect.all
                    [ \m -> Expect.equal (minor ( Row 0, Col 0 ) m) -12
                    , \m -> Expect.equal (cofactor ( Row 0, Col 0 ) m) -12
                    , \m -> Expect.equal (minor ( Row 1, Col 0 ) m) 25
                    , \m -> Expect.equal (cofactor ( Row 1, Col 0 ) m) -25
                    ]
                    a
            )
        , test "Calculating the determinant of a 3x3 matrix"
            (\_ ->
                let
                    a =
                        threeByThree
                            (ThreeColumnRow 1 2 6)
                            (ThreeColumnRow -5 8 -4)
                            (ThreeColumnRow 2 6 4)
                in
                Expect.all
                    [ \m -> Expect.equal (cofactor ( Row 0, Col 0 ) m) 56
                    , \m -> Expect.equal (cofactor ( Row 0, Col 1 ) m) 12
                    , \m -> Expect.equal (cofactor ( Row 0, Col 2 ) m) -46
                    , \m -> Expect.equal (determinant m) -196
                    ]
                    a
            )
        , test "Calculating the determinant of a 4x4 matrix"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow -2 -8 3 5)
                            (FourColumnRow -3 1 7 3)
                            (FourColumnRow 1 2 -9 6)
                            (FourColumnRow -6 7 7 -9)
                in
                Expect.all
                    [ \m -> Expect.equal (cofactor ( Row 0, Col 0 ) m) 690
                    , \m -> Expect.equal (cofactor ( Row 0, Col 1 ) m) 447
                    , \m -> Expect.equal (cofactor ( Row 0, Col 2 ) m) 210
                    , \m -> Expect.equal (cofactor ( Row 0, Col 3 ) m) 51
                    , \m -> Expect.equal (determinant m) -4071
                    ]
                    a
            )
        , test "Testing an invertible matrix for invertibility"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow 6 4 4 4)
                            (FourColumnRow 5 5 7 6)
                            (FourColumnRow 4 -9 3 -7)
                            (FourColumnRow 9 1 7 -6)
                in
                Expect.all
                    [ \m -> Expect.equal (determinant m) -2120
                    , \m -> Expect.equal (isInvertible m) True
                    ]
                    a
            )
        , test "Testing a noninvertible matrix for invertibility"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow -4 2 -2 -3)
                            (FourColumnRow 9 6 2 6)
                            (FourColumnRow 0 -5 1 -5)
                            (FourColumnRow 0 0 0 0)
                in
                Expect.all
                    [ \m -> Expect.equal (determinant m) 0
                    , \m -> Expect.equal (isInvertible m) False
                    ]
                    a
            )
        , test "Calculating the inverse of a matrix"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow -5 2 6 -8)
                            (FourColumnRow 1 -5 1 8)
                            (FourColumnRow 7 7 -6 -7)
                            (FourColumnRow 1 -3 7 4)

                    expectedB =
                        fourByFour
                            (FourColumnRow 0.21805 0.45113 0.2406 -0.04511)
                            (FourColumnRow -0.80827 -1.45677 -0.44361 0.52068)
                            (FourColumnRow -0.07895 -0.22368 -0.05263 0.19737)
                            (FourColumnRow -0.52256 -0.81391 -0.30075 0.30639)
                in
                Expect.all
                    [ \a_ ->
                        let
                            b =
                                invert a_
                        in
                        Expect.all
                            [ \b_ -> Expect.equal (at ( Row 3, Col 2 ) b_) (Just (-160 / 532))
                            , \b_ -> Expect.equal (at ( Row 2, Col 3 ) b_) (Just (105 / 532))
                            , \b_ -> Expect.equal (Lib.Matrix.equal b_ expectedB) True
                            ]
                            b
                    , \a_ -> Expect.equal (determinant a_) 532
                    , \a_ -> Expect.equal (cofactor ( Row 2, Col 3 ) a_) -160
                    , \a_ -> Expect.equal (cofactor ( Row 3, Col 2 ) a_) 105
                    ]
                    a
            )
        , test "Calculating the inverse of another matrix"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow 8 -5 9 2)
                            (FourColumnRow 7 5 6 1)
                            (FourColumnRow -6 0 9 6)
                            (FourColumnRow -3 0 -9 -4)

                    inverted =
                        invert a

                    expectedResult =
                        fourByFour
                            (FourColumnRow -0.15385 -0.15385 -0.28205 -0.53846)
                            (FourColumnRow -0.07692 0.12308 0.02564 0.03077)
                            (FourColumnRow 0.35897 0.35897 0.4359 0.92308)
                            (FourColumnRow -0.69231 -0.69231 -0.76923 -1.92308)
                in
                Expect.equal (Lib.Matrix.equal inverted expectedResult) True
            )
        , test "Calculating the inverse of a third matrix"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow 9 3 0 9)
                            (FourColumnRow -5 -2 -6 -3)
                            (FourColumnRow -4 9 6 4)
                            (FourColumnRow -7 6 6 2)

                    inverted =
                        invert a

                    expectedResult =
                        fourByFour
                            (FourColumnRow -0.04074 -0.07778 0.14444 -0.22222)
                            (FourColumnRow -0.07778 0.03333 0.36667 -0.33333)
                            (FourColumnRow -0.02901 -0.1463 -0.10926 0.12963)
                            (FourColumnRow 0.17778 0.06667 -0.26667 0.33333)
                in
                Expect.equal (Lib.Matrix.equal inverted expectedResult) True
            )
        , test "Multiplying a product by its inverse"
            (\_ ->
                let
                    a =
                        fourByFour
                            (FourColumnRow 3 -9 7 3)
                            (FourColumnRow 3 -8 2 -9)
                            (FourColumnRow -4 4 4 1)
                            (FourColumnRow -6 5 -1 1)

                    b =
                        fourByFour
                            (FourColumnRow 8 2 2 2)
                            (FourColumnRow 3 -1 7 0)
                            (FourColumnRow 7 0 5 4)
                            (FourColumnRow 6 -2 0 5)

                    c =
                        Lib.Matrix.multiply a b
                in
                Expect.equal (Lib.Matrix.equal (Lib.Matrix.multiply c (invert b)) a) True
            )
        ]


checkNumberAt : ( Int, Int ) -> Float -> Matrix a -> Expect.Expectation
checkNumberAt ( r, c ) expected matrix =
    at ( Row r, Col c ) matrix |> Expect.equal (Just expected)
