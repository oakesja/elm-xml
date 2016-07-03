module Test.Xml.Decode exposing (tests)

import String
import ElmTest exposing (Test, suite, test, assertEqual)
import Xml.Decode exposing (..)


tests : Test
tests =
    suite "Xml.Decode"
        [ stringTest
        ]


stringTest : Test
stringTest =
    suite "string"
        [ test "valid string"
            <| assertEqual (Ok "value")
            <| decodeString ("key" := string) "<key>value</key>"
        ]
