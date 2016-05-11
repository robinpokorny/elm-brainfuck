module Main (..) where

import Signal exposing (Signal)
import ElmTest exposing (consoleRunner, suite)
import Console exposing (IO, run)
import Task
import ParserTests
import TapeTests
import UtilsTests


console : IO ()
console =
  [ TapeTests.all
  , ParserTests.all
  , UtilsTests.all
  ]
    |> suite "Brainfuck compiler"
    |> consoleRunner


port runner : Signal (Task.Task x ())
port runner =
  run console
