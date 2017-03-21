module BrainfuckTests exposing (all)

import Test exposing (..)
import Expect
import String
import Char
import Brainfuck


all : Test
all =
    describe "Brainfuck"
        [ empty
        , zero
        , helloWord
        , echoTill255
        , echoTill0
        , multiplier
        ]


empty : Test
empty =
    test "runs empty program" <|
        \() ->
            Expect.equal
                ""
                (Brainfuck.run "" "")


zero : Test
zero =
    let
        instructions =
            (String.repeat 48 "+") ++ "."
    in
        test "runs empty program" <|
            \() ->
                Expect.equal
                    "0"
                    (Brainfuck.run instructions "")


helloWord : Test
helloWord =
    let
        instructions =
            "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."
    in
        test "runs Hello wors" <|
            \() ->
                Expect.equal
                    "Hello World!"
                    (Brainfuck.run instructions "")


echoTill255 : Test
echoTill255 =
    let
        instructions =
            ",+[-.,+]"

        input =
            "Brainfuck" ++ (intToString 255)
    in
        test "echoes until byte(255) encountred" <|
            \() ->
                Expect.equal
                    "Brainfuck"
                    (Brainfuck.run instructions input)


echoTill0 : Test
echoTill0 =
    let
        instructions =
            ",[.[-],]"

        input =
            "Brainfuck" ++ (intToString 0)
    in
        test "echoes until byte(0) encountred" <|
            \() ->
                Expect.equal
                    "Brainfuck"
                    (Brainfuck.run instructions input)


multiplier : Test
multiplier =
    let
        instructions =
            ",>,<[>[->+>+<<]>>[-<<+>>]<<<-]>>."

        input =
            (intToString 7) ++ (intToString 8)
    in
        test "multiplies two numbers" <|
            \() ->
                Expect.equal
                    (intToString 56)
                    (Brainfuck.run instructions input)


intToString : Int -> String
intToString number =
    number
        |> Char.fromCode
        |> String.fromChar
