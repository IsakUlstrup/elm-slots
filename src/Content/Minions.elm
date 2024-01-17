module Content.Minions exposing (beaver, debug)

import Minion exposing (Minion)


debug : Minion
debug =
    Minion.new '🐛' Minion.Debug


beaver : Minion
beaver =
    Minion.new '🦫' Minion.Woodcutting
