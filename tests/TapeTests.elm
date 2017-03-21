module TapeTests exposing (all)

import Test exposing (..)
import Expect
import Dict
import Brainfuck.Tape as Tape


all : Test
all =
    describe "Tape"
        [ empty
        , fromList
        , get
        , set
        , move
        , next
        , prev
        , increment
        , decrement
        ]


empty : Test
empty =
    let
        ( tape, position ) =
            Tape.empty
    in
        describe "empty function"
            [ test "sets position to zero" <|
                \() ->
                    Expect.equal 0 position
            , test "creates empty tape" <|
                \() ->
                    Expect.true "Dict is empty" (Dict.isEmpty tape)
            ]


fromList : Test
fromList =
    let
        tape =
            Tape.fromList [ 4, 3, 500, -50 ]
    in
        describe "fromList function"
            [ test "creates zero based tape" <|
                \() ->
                    Expect.equal 0 (Tuple.second tape)
            , test "assigns respective values" <|
                \() ->
                    Expect.equal 4 (Tape.get tape)
            , test "clamps values over 255" <|
                \() ->
                    Expect.equal 255 (Tape.get (Tape.move 2 tape))
            , test "clamps values bellow 0" <|
                \() ->
                    Expect.equal 0 (Tape.get (Tape.move 3 tape))
            ]


get : Test
get =
    let
        single =
            ( Dict.singleton 1 5, 1 )
    in
        describe "get function"
            [ test "returns zero by default" <|
                \() ->
                    Expect.equal 0 (Tape.get Tape.empty)
            , test "return current value" <|
                \() ->
                    Expect.equal 5 (Tape.get single)
            ]


set : Test
set =
    let
        new =
            Tape.set 5 Tape.empty

        single =
            Tape.set 8 ( Dict.singleton 1 5, 1 )

        max =
            Tape.set 400 Tape.empty

        min =
            Tape.set -10 Tape.empty
    in
        describe "set function"
            [ test "inserts value to current position" <|
                \() ->
                    Expect.equal 5 (Tape.get new)
            , test "overwrites current value" <|
                \() ->
                    Expect.equal 8 (Tape.get single)
            , test "does not allow values over 255" <|
                \() ->
                    Expect.equal 255 (Tape.get max)
            , test "does not allow values bellow 0" <|
                \() ->
                    Expect.equal 0 (Tape.get min)
            ]


move : Test
move =
    describe "move function"
        [ test "moves position to any number" <|
            \() ->
                Expect.equal 30 (Tuple.second (Tape.move 30 Tape.empty))
        , test "moves position even to negative number" <|
            \() ->
                Expect.equal -30 (Tuple.second (Tape.move -30 Tape.empty))
        ]


next : Test
next =
    let
        sample =
            ( Dict.fromList [ ( 0, 4 ), ( 1, 8 ) ], 0 )

        ( tape, position ) =
            Tape.next Tape.empty
    in
        describe "next function"
            [ test "moves position up by one" <|
                \() ->
                    Expect.equal 1 position
            ]


prev : Test
prev =
    let
        ( tape, position ) =
            Tape.prev ( Dict.fromList [ ( 0, 5 ), ( 1, 6 ) ], 1 )
    in
        describe "prev function"
            [ test "moves position down by one" <|
                \() ->
                    Expect.equal 0 position
            , test "moves position down to negative numbers" <|
                \() ->
                    Expect.equal -1 (Tuple.second (Tape.prev Tape.empty))
            ]


increment : Test
increment =
    let
        new =
            Tape.increment Tape.empty

        single =
            Tape.increment ( Dict.singleton 0 5, 0 )

        max =
            Tape.increment ( Dict.singleton 0 255, 0 )
    in
        describe "increment function"
            [ test "adds 1 to current value" <|
                \() ->
                    Expect.equal 6 (Tape.get single)
            , test "sets value to 1 if current value is empty" <|
                \() ->
                    Expect.equal 1 (Tape.get new)
            , test "does not overflow 255" <|
                \() ->
                    Expect.equal 0 (Tape.get max)
            ]


decrement : Test
decrement =
    let
        new =
            Tape.decrement Tape.empty

        single =
            Tape.decrement ( Dict.singleton 0 5, 0 )

        min =
            Tape.decrement ( Dict.singleton 1 0, 1 )
    in
        describe "decrement function"
            [ test "subtacts 1 to current value" <|
                \() ->
                    Expect.equal 4 (Tape.get single)
            , test "sets value to 255 if current value is empty" <|
                \() ->
                    Expect.equal 255 (Tape.get new)
            , test "does not go bellow 0" <|
                \() ->
                    Expect.equal 255 (Tape.get min)
            ]
