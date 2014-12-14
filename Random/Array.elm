module Random.Array where

{-|

# Random Sampling of Arrays

These implementations are good enough for simple games, but should not be used
when statisically sound randomness is required.

@docs sample choose shuffle

-}

import Random
import Random (Generator, Seed)
import Array
import Array (Array)

import Dict
import List ((::))
import Trampoline as T


{-| Sample with replacement: Produce a randomly selected element of the
array and the new seed. -}
sample : Array a -> Seed -> (Maybe a, Seed)
sample arr seed =
    let length = Array.length arr
        intGen = Random.int 0 (length - 1)
        (index, seed') = Random.generate intGen seed
    in (Array.get index arr, seed')

{-| Sample without replacement: Produce a randomly selected element of the
array, the array with that element omitted (shifting all later elements down),
and the new seed. -}
choose : Array a -> Seed -> (Maybe a, Array a, Seed)
choose arr seed = if arr == Array.empty then (Nothing, arr, seed) else
    let intGen = Random.int 0 (Array.length arr - 1)
        (index, seed') = Random.generate intGen seed
        arr' = Array.append (Array.slice 0 index arr) (Array.slice (index+1) (Array.length arr) arr)
    in (Array.get index arr, arr', seed')

type alias ShuffleState a = (Random.Seed, List a, Array a)

isNothing mybe = case mybe of
    Nothing -> True
    _ -> False

{-| Shuffle the array using the Fisher-Yates algorithm. Takes O(_n_ log _n_)
time and O(_n_) additional space. -}
shuffle : Random.Seed -> Array a -> (Array a, Random.Seed)
shuffle seed arr = if arr == Array.empty then (arr, seed) else
    let helper : ShuffleState a -> T.Trampoline (ShuffleState a)
        helper (s, xs, a) = let (m_val, a', s') = choose a s
            in case m_val of
                Nothing -> T.Done (s, xs, a)
                Just val -> T.Continue (\() -> helper (s', val::xs, a'))
        (seed', shuffled, _) = T.trampoline (helper (seed, [], arr))
    in (Array.fromList shuffled, seed')

