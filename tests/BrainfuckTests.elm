module BrainfuckTests (all) where

import ElmTest exposing (..)
import String
import Brainfuck


all : Test
all =
  suite
    "Brainfuck"
    [ empty
    , zero
    , helloWord
    ]


empty : Test
empty =
  test
    "runs empty program"
    (assertEqual "" (Brainfuck.run "" ""))


zero : Test
zero =
  let
    instructions =
      (String.repeat 48 "+") ++ "."
  in
    test
      "runs empty program"
      (assertEqual "0" (Brainfuck.run instructions ""))


helloWord : Test
helloWord =
  let
    instructions =
      "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."
  in
    test
      "runs Hello wors"
      (assertEqual "Hello World!" (Brainfuck.run instructions ""))
