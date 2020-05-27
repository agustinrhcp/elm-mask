module Mask exposing (Pattern(..), Token(..), mask, unMask)

import Html exposing (Attribute)
import Html.Attributes as Attributes


type Pattern
    = Pattern (List Token)


type Token
    = Symbol Char
    | Character
    | Digit


phonePattern : Pattern
phonePattern =
    Pattern [ Symbol '(', Digit, Digit, Digit, Symbol ')' ]


tokens : String -> List Token
tokens string =
    string
        |> String.toList
        |> List.map charToToken


charToToken : Char -> Token
charToToken char =
    case char of
        '#' ->
            Digit

        'X' ->
            Character

        symbol ->
            Symbol symbol


maskValue : String -> Attribute msg
maskValue val =
    if String.isEmpty val then
        Attributes.value ""

    else
        Attributes.value <| mask val "(###)"


mask : String -> String -> String
mask stringInput stringPattern =
    if String.isEmpty stringInput then
        ""

    else
        tokens stringPattern
            |> recMask (String.toList stringInput)


recMask : List Char -> List Token -> String
recMask input pattern =
    case ( input, pattern ) of
        ( x :: xs, y :: ys ) ->
            case y of
                Digit ->
                    case x |> String.fromChar |> String.toInt of
                        Just int ->
                            String.fromInt int ++ recMask xs ys

                        Nothing ->
                            ""

                Character ->
                    String.fromChar x ++ recMask xs ys

                Symbol a ->
                    String.fromChar a ++ recMask input ys

        ( _, [] ) ->
            ""

        ( [], y :: ys ) ->
            case y of
                Symbol char ->
                    String.fromChar char ++ recMask input ys

                _ ->
                    ""


unMask : List Char -> Pattern -> String
unMask input (Pattern pattern) =
    case ( input, pattern ) of
        ( x :: xs, y :: ys ) ->
            case y of
                Digit ->
                    String.fromChar x ++ unMask xs (Pattern ys)

                Character ->
                    String.fromChar x ++ unMask xs (Pattern ys)

                Symbol char ->
                    if char == x then
                        unMask xs (Pattern ys)

                    else
                        ""

        ( _, [] ) ->
            ""

        ( [], _ ) ->
            ""
