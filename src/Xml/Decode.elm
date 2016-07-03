module Xml.Decode exposing (decodeString, (:=), string)

import Combine exposing (Parser, parse, many, between, map)
import Combine.Char exposing (noneOf)
import String


type alias Decoder a =
    Parser a


decodeString : Decoder a -> String -> Result String a
decodeString decoder string =
    parse decoder string
        |> fst
        |> Result.formatError (\e -> "Failed to parse" ++ string)


(:=) : String -> Decoder a -> Decoder a
(:=) key decoder =
    element (Combine.string key) decoder


string : Decoder String
string =
    map String.fromList
        <| many (noneOf [ '<', '>' ])


startTag : Decoder String -> Decoder String
startTag keyDecoder =
    between (Combine.string "<") (Combine.string ">") keyDecoder


endTag : Decoder String -> Decoder String
endTag keyDecoder =
    between (Combine.string "</") (Combine.string ">") keyDecoder


element : Decoder String -> Decoder a -> Decoder a
element keyDecoder valueDecoder =
    between (startTag keyDecoder) (endTag keyDecoder) valueDecoder
