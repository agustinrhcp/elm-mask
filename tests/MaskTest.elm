module MaskTest exposing (suite)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect
import Mask exposing (Pattern(..), Token(..))
import Test exposing (Test, describe, test)


phonePattern : String
phonePattern =
    "(###)"


phonePatternPatter : Pattern
phonePatternPatter =
    Pattern [ Symbol '(', Digit, Digit, Digit, Symbol ')' ]


suite : Test
suite =
    describe "Mask module"
        [ describe "Mask.mask"
            [ test "with an empty string starts applying the mask" <|
                \_ ->
                    Mask.mask "" phonePattern
                        |> Expect.equal ""
            , test "with a couple of digits" <|
                \_ ->
                    Mask.mask "12" phonePattern
                        |> Expect.equal "(12"
            , test "with a complete set of digits" <|
                \_ ->
                    Mask.mask "123" phonePattern
                        |> Expect.equal "(123)"
            , test "with more digits than expected" <|
                \_ ->
                    Mask.mask "1234" phonePattern
                        |> Expect.equal "(123)"
            ]
        , describe "Mask.unMask"
            [ test "with an empty string returns an empty string" <|
                \_ ->
                    Mask.unMask [] phonePatternPatter
                        |> Expect.equal ""
            , test "with a couple of digits" <|
                \_ ->
                    Mask.unMask (String.toList "(12") phonePatternPatter
                        |> Expect.equal "12"
            , test "a complete string" <|
                \_ ->
                    Mask.unMask (String.toList "(123)") phonePatternPatter
                        |> Expect.equal "123"
            , test "with a longer string than expected" <|
                \_ ->
                    Mask.unMask (String.toList "(1234)") phonePatternPatter
                        |> Expect.equal "123"
            ]
        ]
