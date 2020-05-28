module Mask exposing (Pattern, mask, patternFromString, unMask)

import Html exposing (Attribute)
import Html.Attributes as Attributes


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


maskValue : String -> Attribute msg
maskValue val =
    if String.isEmpty val then
        Attributes.value ""

    else
        val
            |> mask (patternFromString "(###)")
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
