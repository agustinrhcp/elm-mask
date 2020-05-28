module MaskTest exposing (suite)

import Expect
import Html
import Html.Attributes as Attributes
import Mask exposing (Pattern, maskedValue, onMaskedInput, patternFromString)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


phonePattern : Pattern
phonePattern =
    patternFromString "(###)"


type Msg
    = Change String


suite : Test
suite =
    describe "Mask module"
        [ describe "Mask.mask"
            [ test "with an empty string starts applying the mask" <|
                \_ ->
                    Mask.mask phonePattern ""
                        |> Expect.equal ""
            , test "with a couple of digits" <|
                \_ ->
                    Mask.mask phonePattern "12"
                        |> Expect.equal "(12"
            , test "with a complete set of digits" <|
                \_ ->
                    Mask.mask phonePattern "123"
                        |> Expect.equal "(123)"
            , test "with more digits than expected" <|
                \_ ->
                    Mask.mask phonePattern "1234"
                        |> Expect.equal "(123)"
            , test "when masking a letter but expecting a digit" <|
                \_ ->
                    Mask.mask phonePattern "12ab"
                        |> Expect.equal "(12"
            ]
        , describe "Mask.unMask"
            [ test "with an empty string returns an empty string" <|
                \_ ->
                    Mask.unMask phonePattern ""
                        |> Expect.equal ""
            , test "with a couple of digits" <|
                \_ ->
                    Mask.unMask phonePattern "(12"
                        |> Expect.equal "12"
            , test "a complete string" <|
                \_ ->
                    Mask.unMask phonePattern "(123)"
                        |> Expect.equal "123"
            , test "with a longer string than expected" <|
                \_ ->
                    Mask.unMask phonePattern "(1234)"
                        |> Expect.equal "123"
            , test "when unmasking a letter but expecting a digit" <|
                \_ ->
                    Mask.unMask phonePattern "(1ab)"
                        |> Expect.equal "1"
            ]
        , describe "Mask.maskValue"
            [ test "with a complete input sets the masked value" <|
                \() ->
                    Html.input [ maskedValue phonePattern "123" ] []
                        |> Query.fromHtml
                        |> Query.has [ Selector.attribute <| Attributes.value "(123)" ]
            , test "with a partial input sets the partial masked value" <|
                \() ->
                    Html.input [ maskedValue phonePattern "1" ] []
                        |> Query.fromHtml
                        |> Query.has [ Selector.attribute <| Attributes.value "(1" ]
            ]
        , describe "Mask.onMaskedInput"
            [ test "when typing" <|
                \() ->
                    Html.input [ Attributes.value "(12", onMaskedInput phonePattern "12" Change ] []
                        |> Query.fromHtml
                        |> Event.simulate (Event.input "(123)")
                        |> Event.expect (Change "123")
            , test "when deleting" <|
                \() ->
                    Html.input [ Attributes.value "(123)", onMaskedInput phonePattern "123" Change ] []
                        |> Query.fromHtml
                        |> Event.simulate (Event.input "(123")
                        |> Event.expect (Change "12")
            , test "when empty" <|
                \() ->
                    Html.input [ Attributes.value "", onMaskedInput phonePattern "" Change ] []
                        |> Query.fromHtml
                        |> Event.simulate (Event.input "1")
                        |> Event.expect (Change "1")
            ]
        ]
