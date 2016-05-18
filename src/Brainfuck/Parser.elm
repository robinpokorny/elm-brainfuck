module Brainfuck.Parser (parse, Command(..), Program) where

{-| Converts Brainfuck source code to Elm structures.

# Definition
@docs Command, Program

# Use
@docs parse

-}

import Dict exposing (Dict)
import String
import Brainfuck.Utils exposing (ensureJust, unionWithReverse)


{-| Parsed Brainfuck program consists of a list of `Command`s to execute
and a symetric `Dict` of jumps of matching square brackets.
-}
type alias Program =
  { commands : List Command
  , loops : Dict Int Int
  }


{-| Represent one of the eight commands of the language.
-}
type Command
  = Next
  | Prev
  | Inc
  | Dec
  | Read
  | Write
  | LoopStart
  | LoopEnd


{-| Parse string to `Program`.

All unknown characters are ignored. Parser assumes correct brackets
pairing and will crash otherwise.

    parse ",[>+<-]>." ==
      { commands = [ Read, LoopStart, Next, … ]
      , loops = Dict.fromList [ ( 1, 6 ), ( 6, 1 ) ]
      }
-}
parse : String -> Program
parse instructions =
  let
    commands =
      instructions
        |> String.toList
        |> List.map toCommands

    loops =
      getLoops commands
  in
    { commands = commands
    , loops = loops
    }


getLoops : List Command -> Dict Int Int
getLoops commands =
  commands
    |> List.indexedMap (,)
    |> List.foldl findLeftMatching ( Dict.empty, [] )
    |> fst
    |> unionWithReverse


findLeftMatching : ( Int, Command ) -> ( Dict Int Int, List Int ) -> ( Dict Int Int, List Int )
findLeftMatching ( index, command ) ( loops, lefts ) =
  case command of
    LoopStart ->
      ( loops, index :: lefts )

    LoopEnd ->
      let
        errorMsg =
          "Brackets do not match."

        leftIndex =
          lefts
            |> List.head
            |> ensureJust errorMsg

        newLoops =
          Dict.insert index leftIndex loops
      in
        ( newLoops, ensureJust errorMsg (List.tail lefts) )

    _ ->
      ( loops, lefts )


toCommands : Char -> Command
toCommands char =
  case char of
    '<' ->
      Next

    '>' ->
      Prev

    '+' ->
      Inc

    '-' ->
      Dec

    '.' ->
      Write

    ',' ->
      Read

    '[' ->
      LoopStart

    ']' ->
      LoopEnd

    _ ->
      Next
