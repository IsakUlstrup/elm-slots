module Suite exposing (inventory)

import Expect
import Fuzz exposing (int)
import Inventory exposing (Slot(..))
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
                    |> Inventory.insert 1 (Item 0)
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Empty )
                        , ( 1, Item 0 )
                        , ( 2, Empty )
                        ]
        , test "Insert item into out of range slot" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 10 (Item 0)
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Empty )
                        , ( 1, Empty )
                        , ( 2, Empty )
                        ]
        , test "Insert item into occupied slot, should not change" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 (Item 0)
                    |> Inventory.insert 0 (Item 10)
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Item 0 )
                        , ( 1, Empty )
                        , ( 2, Empty )
                        ]
        , test "Remove item from valid, occupied slot" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 1 (Item 0)
                    |> Inventory.remove 1
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Empty )
                        , ( 1, Empty )
                        , ( 2, Empty )
                        ]
        , test "Remove item from out of range slot" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 (Item 2)
                    |> Inventory.remove 20
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Item 2 )
                        , ( 1, Empty )
                        , ( 2, Empty )
                        ]
        , test "Get item from occupied slot in range" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 (Item 2)
                    |> Inventory.get 0
                    |> Expect.equal (Just 2)
        , test "Get item from empty slot in range" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.get 0
                    |> Expect.equal Nothing
        , test "Get item from slot out of range" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.get 100
                    |> Expect.equal Nothing
        , test "Switch items, to slot is empty" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 (Item 1)
                    |> Inventory.switch 0 2
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Empty )
                        , ( 1, Empty )
                        , ( 2, Item 1 )
                        ]
        , test "Switch items, both slots are occupied" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 (Item 1)
                    |> Inventory.insert 2 (Item 2)
                    |> Inventory.switch 0 2
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Item 2 )
                        , ( 1, Empty )
                        , ( 2, Item 1 )
                        ]
        , test "Switch items, same index" <|
            \_ ->
                Inventory.new 3
                    |> Inventory.insert 0 (Item 1)
                    |> Inventory.insert 2 (Item 2)
                    |> Inventory.switch 0 0
                    |> Inventory.toList
                    |> Expect.equalLists
                        [ ( 0, Item 1 )
                        , ( 1, Empty )
                        , ( 2, Item 2 )
                        ]
        ]
