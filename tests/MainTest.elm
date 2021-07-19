module MainTest exposing (..)

import Expect
import Fuzz exposing (string)
import Main exposing (..)
import Test exposing (..)


findMaxNumTest : Test
findMaxNumTest =
    let
        bento =
            { dish = "唐揚げ", num = 0 }
    in
    describe "findMaxNum test"
        [ describe "Bento Listが空のとき"
            [ test "最大値が0になる" <|
                \_ ->
                    []
                        |> findMaxNum
                        |> Expect.equal 0
            ]
        , describe "numが3, 1, 2の唐揚げ弁当があるとき"
            [ test "最大値が3になる" <|
                \_ ->
                    [ { bento | num = 3 }, { bento | num = 2 }, { bento | num = 1 } ]
                        |> findMaxNum
                        |> Expect.equal 3
            ]
        , describe "numが1, 1, 1の唐揚げ弁当があるとき"
            [ test "最大値が1になる" <|
                \_ ->
                    [ { bento | num = 1 } ]
                        |> findMaxNum
                        |> Expect.equal 1
            ]
        ]


eatBentoTest : Test
eatBentoTest =
    let
        bento =
            { dish = "唐揚げ", num = 0 }
    in
    describe "eatBento test"
        [ describe "Bento Listが空のとき"
            [ test "空のリストになる" <|
                \_ ->
                    eatBento [] 0
                        |> Expect.equal []
            ]
        , describe "numが3, 2, 1の唐揚げ弁当があるとき" <|
            let
                bentoList =
                    [ { bento | num = 3 }, { bento | num = 2 }, { bento | num = 1 } ]
            in
            [ test "numが2, 2, 1の唐揚げ弁当になる" <|
                \_ ->
                    eatBento bentoList (findMaxNum bentoList)
                        |> Expect.equal [ { bento | num = 2 }, { bento | num = 2 }, { bento | num = 1 } ]
            ]
        , describe "numが2, 2, 1の唐揚げ弁当があるとき" <|
            let
                bentoList =
                    [ { bento | num = 2 }, { bento | num = 2 }, { bento | num = 1 } ]
            in
            [ test "numが1, 2, 1の唐揚げ弁当になる" <|
                \_ ->
                    eatBento bentoList (findMaxNum bentoList)
                        |> Expect.equal [ { bento | num = 1 }, { bento | num = 2 }, { bento | num = 1 } ]
            ]
        ]


updateTest : Test
updateTest =
    let
        bento =
            { dish = "唐揚げ", num = 0 }
    in
    describe "update test"
        [ describe "まだ全てが空っぽでない唐揚げ弁当群を一つ食べると"
            [ test "eatCountが1増え、もっとも唐揚げが多い弁当から一つ唐揚げが減る" <|
                \_ ->
                    { bentoList = [ { bento | num = 3 }, { bento | num = 2 }, { bento | num = 1 } ], eatCount = 0 }
                        |> update Eat
                        |> Expect.equal { bentoList = [ { bento | num = 2 }, { bento | num = 2 }, { bento | num = 1 } ], eatCount = 1 }
            ]
        , describe "全てが空っぽである唐揚げ弁当群を一つ食べようとすると"
            [ test "何も変わらない" <|
                \_ ->
                    { bentoList = [ { bento | num = 0 }, { bento | num = 0 }, { bento | num = 0 } ], eatCount = 6 }
                        |> update Eat
                        |> Expect.equal { bentoList = [ { bento | num = 0 }, { bento | num = 0 }, { bento | num = 0 } ], eatCount = 6 }
            ]
        ]
