module Test.Xml.Decode exposing (tests)

import String
import ElmTest exposing (Test, suite, test, assertEqual)
import Xml.Decode exposing (..)


tests : Test
tests =
    suite "Xml.Decode"
        [ stringTest
        , intTest
        , floatTest
        , boolTest
        , emptyTest
        , listTest
        ]


stringTest : Test
stringTest =
    suite "string"
        [ test "valid string"
            <| assertEqual (Ok "value")
            <| decodeString ("key" := string) "<key>value</key>"
        , test "valid string"
            <| assertEqual (Ok "a string with multiple words")
            <| decodeString ("key" := string) "<key>a string with multiple words</key>"
        ]


intTest : Test
intTest =
    suite "int"
        [ test "positive int"
            <| assertEqual (Ok 123)
            <| decodeString ("key" := int) "<key>123</key>"
        , test "negative int"
            <| assertEqual (Ok -123)
            <| decodeString ("key" := int) "<key>-123</key>"
        ]


floatTest : Test
floatTest =
    suite "float"
        [ test "positive float"
            <| assertEqual (Ok 1.23)
            <| decodeString ("key" := float) "<key>1.23</key>"
        , test "negative float"
            <| assertEqual (Ok -1.23)
            <| decodeString ("key" := float) "<key>-1.23</key>"
        ]


boolTest : Test
boolTest =
    suite "bool"
        [ test "for true"
            <| assertEqual (Ok True)
            <| decodeString ("key" := bool) "<key>true</key>"
        , test "for false"
            <| assertEqual (Ok False)
            <| decodeString ("key" := bool) "<key>false</key>"
        ]


emptyTest : Test
emptyTest =
    suite "empty"
        [ test "when there is nothing between the start and end tag"
            <| assertEqual (Ok 123)
            <| decodeString ("key" := empty 123) "<key></key>"
        , test "when there is only a self-closing tag"
            <| assertEqual (Ok 123)
            <| decodeString ("key" := empty 123) "<key/>"
        ]


listTest : Test
listTest =
    suite "list"
        [ test "valid list"
            <| assertEqual (Ok [ 1, 2, 3 ])
            <| decodeString ("key" := list int) "<key>1 2 3</key>"
        , test "valid list with weird spacing"
            <| assertEqual (Ok [ 1, 2, 3 ])
            <| decodeString ("key" := list int) "<key>  1  2  3    </key>"
        ]
