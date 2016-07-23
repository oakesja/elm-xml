module Test.Xml.Decode exposing (tests)

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
        , atTest
        , maybeTest
        , object1Test
        , object2Test
        , mapTest
        , failTest
        , succeedTest
        ]


stringTest : Test
stringTest =
    suite "string"
        [ test "valid string" <|
            assertEqual (Ok "value") <|
                decodeString ("key" := string) "<key>value</key>"
        , test "valid string" <|
            assertEqual (Ok "a string with multiple words") <|
                decodeString ("key" := string) "<key>a string with multiple words</key>"
        ]


intTest : Test
intTest =
    suite "int"
        [ test "positive int" <|
            assertEqual (Ok 123) <|
                decodeString ("key" := int) "<key>123</key>"
        , test "negative int" <|
            assertEqual (Ok -123) <|
                decodeString ("key" := int) "<key>-123</key>"
        ]


floatTest : Test
floatTest =
    suite "float"
        [ test "positive float" <|
            assertEqual (Ok 1.23) <|
                decodeString ("key" := float) "<key>1.23</key>"
        , test "negative float" <|
            assertEqual (Ok -1.23) <|
                decodeString ("key" := float) "<key>-1.23</key>"
        ]


boolTest : Test
boolTest =
    suite "bool"
        [ test "for true" <|
            assertEqual (Ok True) <|
                decodeString ("key" := bool) "<key>true</key>"
        , test "for false" <|
            assertEqual (Ok False) <|
                decodeString ("key" := bool) "<key>false</key>"
        ]


emptyTest : Test
emptyTest =
    suite "empty"
        [ test "when there is nothing between the start and end tag" <|
            assertEqual (Ok 123) <|
                decodeString ("key" := empty 123) "<key></key>"
          -- , test "when there is only a self-closing tag" <|
          --     assertEqual (Ok 123) <|
          --         decodeString ("key" := empty 123) "<key/>"
        ]


listTest : Test
listTest =
    suite "list"
        [ test "valid list" <|
            assertEqual (Ok [ 1, 2, 3 ]) <|
                decodeString ("key" := list int) "<key>1 2 3</key>"
        , test "valid list with extra spacing" <|
            assertEqual (Ok [ 1, 2, 3 ]) <|
                decodeString ("key" := list int) "<key>  1  2  3    </key>"
        , test "valid list with new lines" <|
            assertEqual (Ok [ 1, 2, 3 ]) <|
                decodeString ("key" := list int) "<key>\n1\n2\n3\n</key>"
        ]


atTest : Test
atTest =
    suite "at"
        [ test "valid path" <|
            assertEqual (Ok "test") <|
                decodeString (at [ "book", "author", "firstname" ] string)
                    """
                    <book>
                        <author>
                            <firstname>test</firstname>
                        </author>
                    </book>
                    """
        ]


object1Test : Test
object1Test =
    let
        createAuthor obj1 =
            { firstname = obj1 }

        decoder =
            object1 createAuthor ("firstname" := string)

        expected =
            createAuthor "john"
    in
        suite "object1"
            [ test "apply the result of a decoder to a function" <|
                assertEqual (Ok expected) <|
                    decodeString decoder
                        """
                        <firstname>john</firstname>
                        """
            ]


object2Test : Test
object2Test =
    let
        createAuthor obj1 obj2 =
            { firstname = obj1
            , lastname = obj2
            }

        decoder =
            object2 createAuthor
                ("firstname" := string)
                ("lastname" := string)

        expected =
            createAuthor "john" "doe"
    in
        suite "object2"
            [ test "apply the result of a decoder to a function" <|
                assertEqual (Ok expected) <|
                    decodeString decoder
                        """
                            <firstname>john</firstname>
                            <lastname>doe</lastname>
                        """
            ]


maybeTest : Test
maybeTest =
    suite "maybe"
        [ test "for non-empty tags" <|
            assertEqual (Ok (Just 123)) <|
                decodeString ("key" := maybe int) "<key>123</key>"
        , test "for empty tags" <|
            assertEqual (Ok Nothing) <|
                decodeString ("key" := maybe int) "<key></key>"
          -- , test "for self-closing tags" <|
          --     assertEqual (Ok Nothing) <|
          --         decodeString ("key" := maybe int) "<key/>"
        ]


mapTest : Test
mapTest =
    suite "map"
        [ test "transform value from decoder" <|
            assertEqual (Ok -1) <|
                decodeString ("key" := map negate int) "<key>1</key>"
        ]


failTest : Test
failTest =
    suite "fail"
        [ test "it fails with the given message" <|
            assertEqual (Err "oops") <|
                decodeString ("key" := fail "oops") "<key>1</key>"
        ]


succeedTest : Test
succeedTest =
    suite "succeed"
        [ test "it succeeds with the given value" <|
            assertEqual (Ok "test") <|
                decodeString ("key" := succeed "test") "<key>1</key>"
        ]
