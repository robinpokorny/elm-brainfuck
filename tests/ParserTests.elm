module ParserTests exposing (all)

import Test exposing (..)
import Expect
import Dict
import Brainfuck.Parser as Parser exposing (Command(..))


all : Test
all =
    describe "Parser"
        [ empty
        , simple
        , loopsSimple
        , loopsComplex
        , ignore
        ]


empty : Test
empty =
    let
        { commands, loops } =
            Parser.parse ""
    in
        describe "empty instructions"
            [ test "create empty commands" <|
                \() ->
                    Expect.true "a" (List.isEmpty commands)
            , test "create empty loops map" <|
                \() ->
                    Expect.true "a" (Dict.isEmpty loops)
            ]


simple : Test
simple =
    let
        { commands } =
            Parser.parse "<>+-.,[]"
    in
        describe "parsers simple instructions"
            [ test "position change" <|
                \() ->
                    Expect.equal [ Prev, Next ] (List.take 2 commands)
            , test "value change" <|
                \() ->
                    Expect.equal [ Inc, Dec ] (List.drop 2 (List.take 4 commands))
            , test "position change" <|
                \() ->
                    Expect.equal [ Write, Read ] (List.drop 4 (List.take 6 commands))
            , test "position change" <|
                \() ->
                    Expect.equal [ LoopStart, LoopEnd ] (List.drop 6 commands)
            ]


loopsSimple : Test
loopsSimple =
    let
        { loops } =
            Parser.parse "[+-]"
    in
        describe "parsers simple loops"
            [ test "single loop start" <|
                \() ->
                    Expect.equal (Just 3) (Dict.get 0 loops)
            , test "single loop end" <|
                \() ->
                    Expect.equal (Just 0) (Dict.get 3 loops)
            ]


loopsComplex : Test
loopsComplex =
    let
        { loops } =
            Parser.parse "[[+-].][]"
    in
        describe "parsers complex loops"
            [ test "outer loop start" <|
                \() ->
                    Expect.equal (Just 6) (Dict.get 0 loops)
            , test "outer loop end" <|
                \() ->
                    Expect.equal (Just 0) (Dict.get 6 loops)
            , test "inner loop start" <|
                \() ->
                    Expect.equal (Just 4) (Dict.get 1 loops)
            , test "inner loop end" <|
                \() ->
                    Expect.equal (Just 1) (Dict.get 4 loops)
            , test "following loop start" <|
                \() ->
                    Expect.equal (Just 8) (Dict.get 7 loops)
            , test "following loop end" <|
                \() ->
                    Expect.equal (Just 7) (Dict.get 8 loops)
            ]


ignore : Test
ignore =
    let
        { commands, loops } =
            Parser.parse "test!"
    in
        describe "ignores unknown instructions"
            [ test "create empty commands" <|
                \() ->
                    Expect.true "a" (List.isEmpty commands)
            , test "create empty loops map" <|
                \() ->
                    Expect.true "a" (Dict.isEmpty loops)
            ]
