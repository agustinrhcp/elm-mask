module Mask exposing (Config, Pattern, mask, maskedValue, onMaskedInput, patternFromString, unMask)

import Html exposing (Attribute)
import Html.Attributes as Attributes
import Html.Events exposing (onInput)


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


maskedValue : Pattern -> String -> Attribute msg
maskedValue pattern val =
    if String.isEmpty val then
        Attributes.value ""

    else
        val
            |> mask pattern
            |> Attributes.value


patternFromStringWithConf : Config -> String -> Pattern
patternFromStringWithConf conf string =
    Pattern <| tokens conf string


patternFromString : String -> Pattern
patternFromString =
    patternFromStringWithConf defaultConfig


mask : Pattern -> String -> String
mask (Pattern pattern) stringInput =
    if String.isEmpty stringInput then
        ""

    else
        pattern
            |> maskRec (String.toList stringInput)


onMaskedInput : Pattern -> String -> (String -> msg) -> Attribute msg
onMaskedInput pattern currentValue msg =
    onInput (unMaskValue pattern currentValue >> msg)


unMaskValue : Pattern -> String -> String -> String
unMaskValue pattern currentValue newValue =
    if String.isEmpty newValue then
        ""

    else if mask pattern currentValue > newValue then
        newValue |> unMask pattern |> String.dropRight 1

    else
        unMask pattern newValue


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


unMask : Pattern -> String -> String
unMask (Pattern pattern) string =
    pattern
        |> unMaskRec (String.toList string)


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
