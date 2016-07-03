module Tests exposing (main)

import ElmTest exposing (Test, runSuite, suite)
import Test.Xml.Decode


tests : Test
tests =
    suite "All Tests"
        [ Test.Xml.Decode.tests
        ]


main : Program Never
main =
    runSuite tests
