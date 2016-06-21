module Main exposing (..)

import ElmTest exposing (..)
import Task
import BrainfuckTests
import ParserTests
import TapeTests
import UtilsTests


tests : Test
tests =
    suite "Brainfuck compiler"
        [ TapeTests.all
        , ParserTests.all
        , UtilsTests.all
        , BrainfuckTests.all
        ]


main : Program Never
main =
    runSuite tests
