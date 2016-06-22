module Brainfuck.Parser exposing (parse, Command(..), Prog)

{-| Converts Brainfuck source code to Elm structures.

# Definition
@docs Command, Prog

# Use
@docs parse

-}

import Dict exposing (Dict)
import String
import Brainfuck.Utils exposing (ensureJust, unionWithReverse)


{-| Parsed Brainfuck program consists of a list of `Command`s to execute
and a symetric `Dict` of jumps of matching square brackets.
-}
type alias Prog =
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


{-| Parse string to `Prog`.

All unknown characters are ignored. Parser assumes correct brackets
pairing and will crash otherwise.

    parse ",[>+<-]>." ==
      { commands = [ Read, LoopStart, Next, … ]
      , loops = Dict.fromList [ ( 1, 6 ), ( 6, 1 ) ]
      }
-}
parse : String -> Prog
parse instructions =
    let
        commands =
            instructions
                |> String.toList
                |> List.map toCommands
                |> List.filter isJust
                |> List.map (Maybe.withDefault Write)

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


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


toCommands : Char -> Maybe Command
toCommands char =
    case char of
        '>' ->
            Just Next

        '<' ->
            Just Prev

        '+' ->
            Just Inc

        '-' ->
            Just Dec

        '.' ->
            Just Write

        ',' ->
            Just Read

        '[' ->
            Just LoopStart

        ']' ->
            Just LoopEnd

        _ ->
            Nothing
