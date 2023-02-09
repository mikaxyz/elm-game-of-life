module Test.GameOfLife exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GameOfLife
import Test exposing (..)



{--
- Any live cell with fewer than two live neighbours dies, as if by underpopulation.
- Any live cell with two or three live neighbours lives on to the next generation.
- Any live cell with more than three live neighbours dies, as if by overpopulation.
- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
--}


suite : Test
suite =
    describe "game of life"
        [ test "cell without neighbours is dead" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells []

                    cell =
                        GameOfLife.cellAtPosition 0 0
                in
                GameOfLife.isAlive world cell
                    |> Expect.equal False
        , test "alive cell with 1 neighbour is dead" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition 0 0
                            , GameOfLife.cellAtPosition 1 0
                            ]

                    cell =
                        GameOfLife.cellAtPosition 1 0
                in
                GameOfLife.isAlive world cell
                    |> Expect.equal False
        , test "alive cell with 2 neighbours is alive" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition 0 0
                            , GameOfLife.cellAtPosition 1 0
                            , GameOfLife.cellAtPosition 2 0
                            ]

                    cell =
                        GameOfLife.cellAtPosition 1 0
                in
                GameOfLife.isAlive world cell
                    |> Expect.equal True
        , test "alive cell with 3 neighbours is alive" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition 0 0
                            , GameOfLife.cellAtPosition 1 0
                            , GameOfLife.cellAtPosition 2 0
                            , GameOfLife.cellAtPosition 1 1
                            ]

                    cell =
                        GameOfLife.cellAtPosition 1 0
                in
                GameOfLife.isAlive world cell
                    |> Expect.equal True
        , test "alive cell with 4 neighbours is dead" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition 0 0
                            , GameOfLife.cellAtPosition 1 0
                            , GameOfLife.cellAtPosition 0 1
                            , GameOfLife.cellAtPosition 1 1
                            , GameOfLife.cellAtPosition -1 0
                            ]

                    cell =
                        GameOfLife.cellAtPosition 0 0
                in
                GameOfLife.isAlive world cell
                |> Expect.equal False
        , test "dead cell with two neighbours is dead" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition -1 0
                            , GameOfLife.cellAtPosition 1 0
                            ]

                    cell =
                        GameOfLife.cellAtPosition 0 0
                in
                GameOfLife.isAlive world cell
                |> Expect.equal False
        , test "dead cell with three neighbours is alive" <|
            \_ ->
                let
                    world =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition 1 1
                            , GameOfLife.cellAtPosition 1 0
                            , GameOfLife.cellAtPosition 0 1
                            ]

                    cell =
                        GameOfLife.cellAtPosition 0 0
                in
                GameOfLife.isAlive world cell
                |> Expect.equal True

        -- RUN
        , test "does blinker" <|
            \_ ->
                let
                    init =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition -1 0
                            , GameOfLife.cellAtPosition 0 0
                            , GameOfLife.cellAtPosition 1 0
                            ]

                    expected =
                        GameOfLife.worldWithCells
                            [ GameOfLife.cellAtPosition 0 -1
                            , GameOfLife.cellAtPosition 0 0
                            , GameOfLife.cellAtPosition 0 1
                            ]
                            |> GameOfLife.toList
                            |> List.sort
                in
                GameOfLife.run init
                    |> GameOfLife.toList
                    |> List.sort
                    |> Expect.equalLists expected
        ]
