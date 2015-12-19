module ListUtil (shuffle) where

{-| Provides useful functions for dealing with Lists.

# Helpers
@docs shuffle
-}

import Array exposing (Array)
import Random.Array
import Random exposing (Seed)


{-| Randomly reorder the elements of a list.
-}
shuffle : Seed -> List a -> (List a, Seed)
shuffle seed list =
    let
        (newList, newSeed) =
            list
                |> Array.fromList
                |> Random.Array.shuffle seed
    in
        (Array.toList newList, newSeed)
