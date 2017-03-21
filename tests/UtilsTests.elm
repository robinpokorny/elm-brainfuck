module UtilsTests exposing (all)

import Test exposing (..)
import Expect
import Dict
import Brainfuck.Utils as Utils


all : Test
all =
    describe "Utils"
        [ ensureJust
        , unionWithReverse
        ]


ensureJust : Test
ensureJust =
    describe "ensureJust function"
        [ test "returns value when present" <|
            \() ->
                Expect.equal
                    5
                    (Utils.ensureJust "Error" (Just 5))
        ]


unionWithReverse : Test
unionWithReverse =
    let
        result =
            Utils.unionWithReverse (Dict.singleton 0 1)
    in
        describe "unionWithReverse function"
            [ test "keeps original values" <|
                \() ->
                    Expect.equal
                        (Just 1)
                        (Dict.get 0 result)
            , test "include values from reverse" <|
                \() ->
                    Expect.equal
                        (Just 0)
                        (Dict.get 1 result)
            ]
