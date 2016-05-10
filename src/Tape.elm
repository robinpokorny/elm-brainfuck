module Tape (Tape, empty, fromList, get, set, move, next, prev, increment, decrement) where

{-| Simple unbounded array with a movable data pointer.
Tape is indexed by `Int` and its values are also `Int`

# Tapes
@docs Tape

# Build
@docs empty, fromList

# Pointer
@docs move, next, prev

# Value
@docs get, set, increment, decrement

-}

import Dict


{-| An unbounded `Int`-indexed tape of numbers from 0 to 255.
-}
type alias Tape =
  ( Dict.Dict Int Int, Int )


{-| Create an empty tape.
-}
empty : Tape
empty =
  ( Dict.empty, 0 )


{-| Convert a list into a tape.
-}
fromList : List Int -> Tape
fromList values =
  let
    clamped =
      List.map clampValue values

    tape =
      Dict.fromList (List.indexedMap (,) clamped)
  in
    ( tape, 0 )


{-| Get the value at the current position. If the key is not found, return
0.
-}
get : Tape -> Int
get ( tape, position ) =
  Maybe.withDefault 0 (Dict.get position tape)


{-| Set value at current position. Value is clamped within 0 and 255.
-}
set : Int -> Tape -> Tape
set value ( tape, position ) =
  let
    newTape =
      Dict.insert position (clampValue value) tape
  in
    ( newTape, position )


{-| Set position.
-}
move : Int -> Tape -> Tape
move position ( tape, _ ) =
  ( tape, position )


{-| Move position right by one.
-}
next : Tape -> Tape
next ( tape, position ) =
  ( tape, position + 1 )


{-| Move position left by one.
-}
prev : Tape -> Tape
prev ( tape, position ) =
  ( tape, position - 1 )


addOne : Maybe Int -> Maybe Int
addOne value =
  Just
    (case value of
      Just n ->
        if n < 255 then
          n + 1
        else
          0

      Nothing ->
        1
    )


{-| Increment value at current position by one. Value is truncated overflow.

    tape = fromList [ 255 ]

    get (increment tape) == 0
-}
increment : Tape -> Tape
increment ( tape, position ) =
  let
    newTape =
      Dict.update position addOne tape
  in
    ( newTape, position )


subtractOne : Maybe Int -> Maybe Int
subtractOne value =
  Just
    (case value of
      Just n ->
        if n > 0 then
          n - 1
        else
          255

      Nothing ->
        255
    )


{-| Decrement value at current position by one. Value is floored at 0.

    get (decrement empty) == 255
-}
decrement : Tape -> Tape
decrement ( tape, position ) =
  let
    newTape =
      Dict.update position subtractOne tape
  in
    ( newTape, position )


clampValue : Int -> Int
clampValue value =
  (clamp 0 255 value)
