module UtilsTests exposing (all)

import ElmTest exposing (..)
import Dict
import Brainfuck.Utils as Utils


all : Test
all =
    suite "Utils"
        [ ensureJust
        , unionWithReverse
        ]


ensureJust : Test
ensureJust =
    suite "ensureJust function"
        [ test "returns value when present"
            (assertEqual 5 (Utils.ensureJust "Error" (Just 5)))
        ]


unionWithReverse : Test
unionWithReverse =
    let
        result =
            Utils.unionWithReverse (Dict.singleton 0 1)
    in
        suite "unionWithReverse function"
            [ test "keeps original values"
                (assertEqual (Just 1) (Dict.get 0 result))
            , test "include values from reverse"
                (assertEqual (Just 0) (Dict.get 1 result))
            ]
