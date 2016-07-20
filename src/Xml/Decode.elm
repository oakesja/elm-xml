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
        , at
        , maybe
        , map
        , fail
        , succeed
        )

import Combine exposing (Parser, parse, many, between, map, andThen, succeed, or, skipMany)
import Combine.Char exposing (noneOf, space, oneOf)
import Combine.Num
import String


type alias Decoder a =
    Parser a



-- TODO
-- empty tags
-- prolog
-- self closing tags
-- xpath
-- error handling
-- Run a decoder


decodeString : Decoder a -> String -> Result String a
decodeString decoder string =
    parse decoder string
        |> fst
        |> Result.formatError (String.join " ")



-- Primatives


string : Decoder String
string =
    map String.fromList anythingButTags


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



-- Objects
-- at
-- object1-8
-- keyValuePairs
-- dict


list : Decoder a -> Decoder (List a)
list decoder =
    many <|
        between (skipMany whitespace) (skipMany whitespace) decoder


(:=) : String -> Decoder a -> Decoder a
(:=) key decoder =
    element (Combine.string key) decoder


at : List String -> Decoder a -> Decoder a
at keys decoder =
    List.foldr (\key d -> element (Combine.string key) d) decoder keys



-- Oddly shaped values
-- oneOf
-- andThen
-- customDecoder


maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    Combine.maybe decoder


map : (a -> b) -> Decoder a -> Decoder b
map mapFunc decoder =
    Combine.map mapFunc decoder


fail : String -> Decoder a
fail failureMsg =
    Combine.fail [ failureMsg ]


succeed : a -> Decoder a
succeed value =
    anythingButTags `andThen` (\_ -> Combine.succeed value)



-- private


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


whitespace : Decoder Char
whitespace =
    oneOf [ ' ', '\n', '\t', '\x0D' ]


anythingButTags : Decoder (List Char)
anythingButTags =
    many (noneOf [ '<', '>' ])
