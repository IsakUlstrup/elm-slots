module Suite exposing (inventory)

import Expect
import Fuzz exposing (int)
import Inventory
import Test exposing (Test, describe, fuzz, test)


inventory : Test
inventory =
    describe "Inventory"
        [ fuzz int "Constructor size fuzzer, values outside 0-10 should be clamped" <|
            \size ->
                Inventory.new size
                    |> Inventory.toList
                    |> List.length
                    |> (\length ->
                            if size < 0 then
                                Expect.equal 0 length

                            else if size > 100 then
                                Expect.equal 100 length

                            else
                                Expect.equal size length
                       )
        , test "Insert item into valid slot" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 1 0
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Nothing )
                        , ( 1, Just 0 )
                        , ( 2, Nothing )
                        ]
        , test "Insert item into out of range slot" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 10 0
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Nothing )
                        , ( 1, Nothing )
                        , ( 2, Nothing )
                        ]
        , test "Insert item into occupied slot, should not change" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 0
                    |> Inventory.insert 0 10
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Just 0 )
                        , ( 1, Nothing )
                        , ( 2, Nothing )
                        ]
        ]
