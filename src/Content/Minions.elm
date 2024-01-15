module Content.Minions exposing (debug)

import Minion exposing (Minion)


debug : Minion
debug =
    Minion.new '🐛' [ ( Minion.Debug, 10 ) ]
