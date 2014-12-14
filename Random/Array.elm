module Random.Array where

{-|

# Random Sampling of Arrays

These implementations are thought to be correct as validated by non-rigorous
tests, and should be fine for games and simulations. Please do not trust them
when statisic or especially cryptographic randomness is required.

For best results, invoke `Random.initialSeed` only once in your program, and
thread the returned new seeds through. Do not pass the initial seed but rather
peel off at least one psuedorandom number first.

@docs sample, choose, shuffle

-}

import Random
import Array
import Dict
import List ((::))
import Trampoline as T

{-| Sample with replacement: Produce a randomly selected element of the
array and the new seed. -}
sample : Random.Seed -> Array.Array a -> (Maybe a, Random.Seed)
sample seed arr =
    let length = Array.length arr
        intGen = Random.int 0 (length - 1)
        (index, seed') = Random.generate intGen seed
    in (Array.get index arr, seed')

{-| Sample without replacement: Produce a randomly selected element of the
array, the array with that element omitted (shifting all later elements down),
and the new seed. -}
choose : Random.Seed -> Array.Array a -> (Maybe a, Random.Seed, Array.Array a)
choose seed arr = if arr == Array.empty then (Nothing, seed, arr) else
    let intGen = Random.int 0 (Array.length arr - 1)
        (index, seed') = Random.generate intGen seed
        arr' = Array.append (Array.slice 0 index arr) (Array.slice (index+1) (Array.length arr) arr)
    in (Array.get index arr, seed', arr')

-- type alias ShuffleState a = (Random.Seed, List a, Array.Array a)

{-| Shuffle the array using the Fisher-Yates algorithm. Takes O(_n_ log _n_)
time and O(_n_) additional space. -}
shuffle : Random.Seed -> Array.Array a -> (Array.Array a, Random.Seed)
shuffle seed arr = if arr == Array.empty then (arr, seed) else
    let -- helper : ShuffleState a -> T.Trampoline (ShuffleState a)
        helper (s, xs, a) = let (m_val, s', a') = choose s a
            in case m_val of
                Nothing -> T.Done (s, xs, a)
                Just val -> T.Continue (\() -> helper (s', val::xs, a'))
        (seed', shuffled, _) = T.trampoline (helper (seed, [], arr))
    in (Array.fromList shuffled, seed')
