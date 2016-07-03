module Xml.Decode
    exposing
        ( decodeString
        , (:=)
        , string
        , int
        , float
        , bool
        , empty
        , list
        )

import Combine exposing (Parser, parse, many, between, map, andThen, succeed, or, skipMany)
import Combine.Char exposing (noneOf, space)
import Combine.Num
import String


type alias Decoder a =
    Parser a


decodeString : Decoder a -> String -> Result String a
decodeString decoder string =
    parse decoder string
        |> fst
        |> Result.formatError
            (\e ->
                let
                    _ =
                        Debug.log "error" e
                in
                    "Failed to parse" ++ string
            )


(:=) : String -> Decoder a -> Decoder a
(:=) key decoder =
    element (Combine.string key) decoder


string : Decoder String
string =
    map String.fromList
        <| many (noneOf [ '<', '>' ])


int : Decoder Int
int =
    Combine.Num.int


float : Decoder Float
float =
    Combine.Num.float


bool : Decoder Bool
bool =
    true `or` false


empty : a -> Decoder a
empty value =
    succeed value


list : Decoder a -> Decoder (List a)
list decoder =
    many
        <| between (skipMany space) (skipMany space) decoder


true : Decoder Bool
true =
    Combine.string "true" `andThen` (\_ -> succeed True)


false : Decoder Bool
false =
    Combine.string "false" `andThen` (\_ -> succeed False)


startTag : Decoder String -> Decoder String
startTag keyDecoder =
    between (Combine.string "<") (Combine.string ">") keyDecoder


endTag : Decoder String -> Decoder String
endTag keyDecoder =
    between (Combine.string "</") (Combine.string ">") keyDecoder


element : Decoder String -> Decoder a -> Decoder a
element keyDecoder valueDecoder =
    between (startTag keyDecoder) (endTag keyDecoder) valueDecoder
