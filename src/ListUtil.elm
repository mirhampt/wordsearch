module ListUtil where

import Array exposing (Array)
import Random.Array
import Random exposing (Seed)


shuffle : Seed -> List a -> (List a, Seed)
shuffle seed list =
    let
        (newList, newSeed) =
            list
                |> Array.fromList
                |> Random.Array.shuffle seed
    in
        (Array.toList newList, newSeed)
