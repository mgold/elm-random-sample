-- This is a reproduction (and regression test) for GitHub Issue #1

import Graphics.Element exposing (show)
import Random
import Array
import Random.Array exposing (choose)

array = Array.fromList [0..36]
seed = Random.initialSeed 48
main = show <| choose seed array
