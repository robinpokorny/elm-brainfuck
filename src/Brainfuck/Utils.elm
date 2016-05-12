module Brainfuck.Utils (ensureJust, unionWithReverse) where

import Dict exposing (Dict)


ensureJust : String -> Maybe a -> a
ensureJust message maybea =
  case maybea of
    Just a ->
      a

    Nothing ->
      Debug.crash message


unionWithReverse : Dict comparable comparable -> Dict comparable comparable
unionWithReverse dictionary =
  let
    reverse =
      dictionary
        |> Dict.toList
        |> List.map (\( a, b ) -> ( b, a ))
        |> Dict.fromList
  in
    Dict.union dictionary reverse
