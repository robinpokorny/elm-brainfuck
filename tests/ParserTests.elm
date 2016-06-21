module ParserTests exposing (all)

import ElmTest exposing (..)
import Dict
import Brainfuck.Parser as Parser exposing (Command(..))


all : Test
all =
    suite "Parser"
        [ empty
        , simple
        , loopsSimple
        , loopsComplex
        ]


empty : Test
empty =
    let
        { commands, loops } =
            Parser.parse ""
    in
        suite "empty instructions"
            [ test "create empty commands"
                (assert (List.isEmpty commands))
            , test "create empty loops map"
                (assert (Dict.isEmpty loops))
            ]


simple : Test
simple =
    let
        { commands } =
            Parser.parse "<>+-.,[]"
    in
        suite "parsers simple instructions"
            [ test "position change"
                (assertEqual [ Next, Prev ] (List.take 2 commands))
            , test "value change"
                (assertEqual [ Inc, Dec ] (List.drop 2 (List.take 4 commands)))
            , test "position change"
                (assertEqual [ Write, Read ] (List.drop 4 (List.take 6 commands)))
            , test "position change"
                (assertEqual [ LoopStart, LoopEnd ] (List.drop 6 commands))
            ]


loopsSimple : Test
loopsSimple =
    let
        { loops } =
            Parser.parse "[+-]"
    in
        suite "parsers simple loops"
            [ test "single loop start"
                (assertEqual (Just 3) (Dict.get 0 loops))
            , test "single loop end"
                (assertEqual (Just 0) (Dict.get 3 loops))
            ]


loopsComplex : Test
loopsComplex =
    let
        { loops } =
            Parser.parse "[[+-].][]"
    in
        suite "parsers complex loops"
            [ test "outer loop start"
                (assertEqual (Just 6) (Dict.get 0 loops))
            , test "outer loop end"
                (assertEqual (Just 0) (Dict.get 6 loops))
            , test "inner loop start"
                (assertEqual (Just 4) (Dict.get 1 loops))
            , test "inner loop end"
                (assertEqual (Just 1) (Dict.get 4 loops))
            , test "following loop start"
                (assertEqual (Just 8) (Dict.get 7 loops))
            , test "following loop end"
                (assertEqual (Just 7) (Dict.get 8 loops))
            ]
