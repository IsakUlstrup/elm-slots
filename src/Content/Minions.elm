module Content.Minions exposing (builder, debug)

import Minion exposing (Minion)


debug : Minion
debug =
    Minion.new '🐛' [ ( Minion.Debug, 10 ) ]


builder : Minion
builder =
    Minion.new '\u{1F9AB}' [ ( Minion.Construction, 10 ) ]
