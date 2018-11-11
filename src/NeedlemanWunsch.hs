module NeedlemanWunsch where

import Data.Array
import Data.List (maximumBy)

data Pair a b = Start
              | GapL b
              | GapR a
              | Match a b

type Alignment a b = [Pair a b]


-- Pointer to the cell whose value contributed to the current one.
-- In contrast to classical N-W, we have unique pointers.
data Source = Origin
            | FromLeft
            | FromTop
            | FromDiag

-- Type of the N-W matrix entry: pointer to source cell and the value
type Entry = (Source, Double)

-- The Needleman-Wunsch dynamic programming algorithm
nw :: [a] -> [b] -> (a -> b -> Double) -> Double -> Alignment a b
nw stream1 stream2 sim gap = walkback (length s1) (length s2) [] where
    -- Convert to 1-based arrays for easy indexing and fast random access
    s1 = listArray (1, length stream1) stream1
    s2 = listArray (1, length stream2) stream2

    -- Fill in the N-W matrix
    fill 0 0 = (Origin, 0.0)
    fill 0 j = (FromLeft, gap*(fromIntegral j))
    fill i 0 = (FromTop, gap*(fromIntegral i))
    fill i j = maximumBy maxVal candidates where
        candidates = [(FromLeft, (snd $ m!(i, j-1)) + gap),
                      (FromTop, (snd $ m!(i-1, j)) + gap),
                      (FromDiag, (snd $ m!(i-1, j-1)) + sim (s1!i) (s2!j))]
        maxVal e1 e2 = case snd e1 < snd e2 of
            True -> LT
            False -> if snd e1 > snd e2 then GT else EQ

    -- Walk through the matrix and reconstruct the best alignment
    walkback 0 0 complete = complete
    walkback i j partial = walkback i' j' (pair:partial) where
        (i', j', pair) = case fst $ m!(i, j) of
            FromLeft -> (i, j-1, GapL $ s2!j)
            FromTop  -> (i-1, j, GapR $ s1!i)
            FromDiag -> (i-1, j-1, Match (s1!i) (s2!j))

    -- The N-W matrix itself
    m = listArray dims [fill i j | (i, j) <- range dims]
    dims = ((0, 0), (length s1, length s2))
