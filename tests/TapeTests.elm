module TapeTests (all) where

import ElmTest exposing (..)
import Dict
import Brainfuck.Tape as Tape


all : Test
all =
  suite
    "Tape"
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
    suite
      "empty function"
      [ test
          "sets position to zero"
          (assertEqual 0 position)
      , test
          "creates empty tape"
          (assert (Dict.isEmpty tape))
      ]


fromList : Test
fromList =
  let
    tape =
      Tape.fromList [ 4, 3, 500, -50 ]
  in
    suite
      "fromList function"
      [ test
          "creates zero based tape"
          (assertEqual 0 (snd tape))
      , test
          "assigns respective values"
          (assertEqual 4 (Tape.get tape))
      , test
          "clamps values over 255"
          (assertEqual 255 (Tape.get (Tape.move 2 tape)))
      , test
          "clamps values bellow 0"
          (assertEqual 0 (Tape.get (Tape.move 3 tape)))
      ]


get : Test
get =
  let
    single =
      ( Dict.singleton 1 5, 1 )
  in
    suite
      "get function"
      [ test
          "returns zero by default"
          (assertEqual 0 (Tape.get Tape.empty))
      , test
          "return current value"
          (assertEqual 5 (Tape.get single))
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
    suite
      "set function"
      [ test
          "inserts value to current position"
          (assertEqual 5 (Tape.get new))
      , test
          "overwrites current value"
          (assertEqual 8 (Tape.get single))
      , test
          "does not allow values over 255"
          (assertEqual 255 (Tape.get max))
      , test
          "does not allow values bellow 0"
          (assertEqual 0 (Tape.get min))
      ]


move : Test
move =
  suite
    "move function"
    [ test
        "moves position to any number"
        (assertEqual 30 (snd (Tape.move 30 Tape.empty)))
    , test
        "moves position even to negative number"
        (assertEqual -30 (snd (Tape.move -30 Tape.empty)))
    ]


next : Test
next =
  let
    sample =
      ( Dict.fromList [ ( 0, 4 ), ( 1, 8 ) ], 0 )

    ( tape, position ) =
      Tape.next Tape.empty
  in
    suite
      "next function"
      [ test
          "moves position up by one"
          (assertEqual 1 position)
      ]


prev : Test
prev =
  let
    ( tape, position ) =
      Tape.prev ( Dict.fromList [ ( 0, 5 ), ( 1, 6 ) ], 1 )
  in
    suite
      "prev function"
      [ test
          "moves position down by one"
          (assertEqual 0 position)
      , test
          "moves position down to negative numbers"
          (assertEqual -1 (snd (Tape.prev Tape.empty)))
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
    suite
      "increment function"
      [ test
          "adds 1 to current value"
          (assertEqual 6 (Tape.get single))
      , test
          "sets value to 1 if current value is empty"
          (assertEqual 1 (Tape.get new))
      , test
          "does not overflow 255"
          (assertEqual 0 (Tape.get max))
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
    suite
      "decrement function"
      [ test
          "subtacts 1 to current value"
          (assertEqual 4 (Tape.get single))
      , test
          "sets value to 255 if current value is empty"
          (assertEqual 255 (Tape.get new))
      , test
          "does not go bellow 0"
          (assertEqual 255 (Tape.get min))
      ]
