module Mask exposing
    ( Pattern
    , fromString
    , mask, unMask
    , maskedValue, onMaskedInput
    )

{-| Simple libreary to mask strings and inputs.


# Pattern

@docs Pattern


# Helpers

@docs fromString


# Masking / Unmasking strings

@docs mask, unMask


# Input helpers

@docs maskedValue, onMaskedInput

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes
import Html.Events exposing (onInput)


{-| Represent the pattern that will be applied to the
string to mask
-}
type Pattern
    = Pattern (List Token)


type Token
    = Symbol Char
    | Any
    | Digit


type alias Config =
    { digitChar : Char
    , anyChar : Char
    }


defaultConfig : Config
defaultConfig =
    { digitChar = '#'
    , anyChar = '*'
    }


tokens : Config -> String -> List Token
tokens conf string =
    string
        |> String.toList
        |> List.map (charToToken conf)


charToToken : Config -> Char -> Token
charToToken { digitChar, anyChar } char =
    if char == digitChar then
        Digit

    else if char == anyChar then
        Any

    else
        Symbol char


fromStringWithConf : Config -> String -> Pattern
fromStringWithConf conf string =
    Pattern <| tokens conf string


{-| Creates a pattern from a String, using


# to match digits and \* for any character.

    phonePattern : Pattern

    phonePatter =
        Mask.fromString "(###)###-####"

-}
fromString : String -> Pattern
fromString =
    fromStringWithConf defaultConfig


{-| Masks a string, useful for displaying formatted data.

    maskPhone : String -> String
    maskPhone phoneNumber =
        Mask.mask phonePattern phoneNumber

    maskPhone "1231231234" == "(123)123-1234"

-}
mask : Pattern -> String -> String
mask (Pattern pattern) stringInput =
    if String.isEmpty stringInput then
        ""

    else
        pattern
            |> maskRec (String.toList stringInput)


{-| Unmasks a string.

    unmaskPhone : String -> String
    unmaskPhone phoneNumber =
        Mask.unMask phonePattern phoneNumber

    maskPhone "(123)123-1234" == "1231231234"

-}
unMask : Pattern -> String -> String
unMask (Pattern pattern) string =
    if String.isEmpty string then
        ""

    else
        pattern
            |> unMaskRec (String.toList string)


{-| Similar to Html.Attributes.value, but applying the provided pattern

    input [ maskedValue phonePattern model.phoneNumber ] []

-}
maskedValue : Pattern -> String -> Attribute msg
maskedValue pattern val =
    if String.isEmpty val then
        Attributes.value ""

    else
        val
            |> mask pattern
            |> Attributes.value


{-| Similar to Html.Events.onInput, but unmasking a masked value.
It requires the previous value as the second argument

    type Msg = SetPhone String

    input
        [ maskedValue phonePattern model.phoneNumber
        , onMaskedValue phonePattern model.phoneNumber SetPhoneNumber
        ]
        []

-}
onMaskedInput : Pattern -> String -> (String -> msg) -> Attribute msg
onMaskedInput pattern currentValue msg =
    onInput (unMaskValue pattern currentValue >> msg)


unMaskValue : Pattern -> String -> String -> String
unMaskValue pattern currentValue maskedNewValue =
    if String.isEmpty maskedNewValue then
        ""

    else if String.isEmpty currentValue then
        maskedNewValue |> mask pattern |> unMask pattern

    else if mask pattern currentValue > maskedNewValue then
        -- When the current masked value's length is > than the new masked value
        -- The new value is being deleted
        let
            unmaskedNewValue =
                unMask pattern maskedNewValue
        in
        if currentValue > unmaskedNewValue then
            unmaskedNewValue

        else
            currentValue |> String.dropRight 1

    else
        unMask pattern maskedNewValue


maskRec : List Char -> List Token -> String
maskRec input pattern =
    case ( input, pattern ) of
        ( char :: remainingChar, token :: remainingTokens ) ->
            case token of
                Digit ->
                    char
                        |> String.fromChar
                        |> String.toInt
                        |> Maybe.map (\int -> String.fromInt int ++ maskRec remainingChar remainingTokens)
                        |> Maybe.withDefault ""

                Any ->
                    String.fromChar char ++ maskRec remainingChar remainingTokens

                Symbol a ->
                    String.fromChar a ++ maskRec input remainingTokens

        ( _, [] ) ->
            ""

        ( [], token :: remainingTokens ) ->
            case token of
                Symbol symbol ->
                    String.fromChar symbol ++ maskRec input remainingTokens

                _ ->
                    ""


unMaskRec : List Char -> List Token -> String
unMaskRec input pattern =
    case ( input, pattern ) of
        ( char :: remainingChar, token :: remainingTokens ) ->
            case token of
                Digit ->
                    char
                        |> String.fromChar
                        |> String.toInt
                        |> Maybe.map (\int -> String.fromInt int ++ unMaskRec remainingChar remainingTokens)
                        |> Maybe.withDefault ""

                Any ->
                    String.fromChar char ++ unMaskRec remainingChar remainingTokens

                Symbol symbol ->
                    if symbol == char then
                        unMaskRec remainingChar remainingTokens

                    else
                        ""

        ( _, [] ) ->
            ""

        ( [], _ ) ->
            ""
