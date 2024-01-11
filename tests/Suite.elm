module Suite exposing (inventory)

import Expect
import Fuzz exposing (int)
import Inventory
import Test exposing (Test, describe, fuzz)


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
        ]
