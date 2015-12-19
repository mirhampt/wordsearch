module ListUtil (shuffle, chunk) where

{-| Provides useful functions for dealing with Lists.

# Helpers
@docs shuffle, chunk
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


{-| Split a list into multiple lists each of the given `length`. The elements
will remain in the same order. Remaining characters, if any, will appear in the
last list.

    ListUtil.chunk 2 [1, 2, 3, 4] == [[1, 2], [3, 4]]
    ListUtil.chunk 2 [1, 2, 3] == [[1, 2], [3]]
-}
chunk : Int -> List a -> List (List a)
chunk length list =
    case list of
        [] -> []
        content -> List.take length content :: chunk length (List.drop length content)
