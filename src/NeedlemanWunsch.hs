module NeedlemanWunsch where

import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.ST as UM
import qualified Data.Array.Unboxed as U
import Data.List (intersperse, sortBy)

-- Unqualified infix indexing operations for boxed and unboxed arrays.
-- Nb. writing (!) = A.! doesn't work.
infixl 9 !
m!i = m A.! i

infixl 9 %
m%i = m U.! i

-- An Alignment is a list of gaps and matches.
data Pair a b = GapL b
              | GapR a
              | Match a b

data Alignment a b = Alignment [Pair a b]

joinAlignments :: Alignment a b -> Alignment a b -> Alignment a b
joinAlignments (Alignment leftPairs) (Alignment rightPairs) = Alignment (leftPairs ++ rightPairs)

-- Print an Alignment line-by-line, summarising gaps.
-- Works best if both a and b print as short one-liners.
instance (Show a, Show b) => Show (Alignment a b) where
    show (Alignment pairs) =
        let -- Figure out the optimal width of the left column
            isMatch (Match _ _) = True
            isMatch _ = False
            matches = filter isMatch pairs
            getLeft (Match x y) = x
            leftWidth = case map (length . show . getLeft) matches of
                [] -> 25
                l -> (maximum l) + 4

            -- Display either a match or a compressed gap in a single line
            padRight n s = s ++ foldl (++) "" (replicate (n - length s) " ")
            showMatch (Match x y) = padRight leftWidth (show x) ++ (show y)
            showGap 0 = ""
            showGap n = "--gap length " ++ show n ++ "--"
            showGaps 0 0 = ""
            showGaps l r = (padRight leftWidth) (showGap l) ++ (showGap r)

            -- Scan the alignment, count gaps, and generate lines of text
            scan (l, r) ((Match x y):rest) = [showGaps l r, showMatch (Match x y)] ++ scan (0, 0) rest
            scan (l, r) [] = [showGaps l r]
            scan (l, r) ((GapL _):rest) = scan (l+1, r) rest
            scan (l, r) ((GapR _):rest) = scan (l, r+1) rest
            desc = scan (0, 0) pairs
        in foldl (++) "" (intersperse "\n" (filter (\s -> s /= "") desc))

-- Compute the total alignment score
alignmentScore :: (a -> b -> Double) -> (b -> Double) -> (a -> Double) -> Alignment a b -> Double
alignmentScore sim gapl gapr (Alignment pairs) = sum (map value pairs) where
    value (GapL y) = gapl y
    value (GapR x) = gapr x
    value (Match x y) = sim x y


-- The Needleman-Wunsch-inspired dynamic alignment algorithm.
align :: [a] -> [b] -> (a -> b -> Double) -> (b -> Double) -> (a -> Double) -> Alignment a b
align stream1 stream2 sim gapl gapr = Alignment (walkback (length s1) (length s2) []) where
    -- Convert data to 1-based arrays for easier indexing and fast random access
    s1 = A.listArray (1, length stream1) stream1
    s2 = A.listArray (1, length stream2) stream2

    -- Compute gap penalties once
    g1 = U.listArray (1, length stream1) (map gapr stream1)  :: U.UArray Int Double
    g2 = U.listArray (1, length stream2) (map gapl stream2)  :: U.UArray Int Double

    -- Create the main matrix. Unboxed mutable array is used: mutabiliy for
    -- inductive definition, unboxedness for reduced memory footprint. O(m*n)
    mbounds = ((0,0), (length s1, length s2))
    m = UM.runSTUArray $ do
        arr <- UM.newArray_ mbounds
        let set = UM.writeArray arr
        let get = UM.readArray arr
        let fill (0, 0) = do set (0, 0) 0.0
            fill (i, 0) = do top <- get (i-1, 0)
                             set (i, 0) (top + (g1%i))
            fill (0, j) = do left <- get (0, j-1)
                             set (0, j) (left + (g2%j))
            fill (i, j) = do top <- get (i-1, j)
                             left <- get (i, j-1)
                             diag <- get (i-1, j-1)
                             let fromTop = top + (g1%i)
                             let fromLeft = left + (g2%j)
                             let fromDiag = diag + (sim (s1!i) (s2!j))
                             set (i, j) (maximum [fromTop, fromLeft, fromDiag])
        forM_ (UM.range mbounds) fill
        return arr

    -- Walk through the matrix and reconstruct the best alignment. O(max(n, k))
    -- Entries on the way are recomputed to avoid the need to keep cell pointers.
    walkback 0 0 complete = complete
    walkback i 0 partial = walkback (i-1) 0 ((GapR (s1!i)):partial)
    walkback 0 j partial = walkback 0 (j-1) ((GapL (s2!j)):partial)
    walkback i j partial = walkback i' j' (pair:partial) where
        (i', j', pair) = (fst . head . reverse . (sortBy comp)) candidates where
            comp = \x -> \y -> compare (snd x) (snd y)
            fromTop = (m%(i-1, j)) + (g1%i)
            fromLeft = (m%(i, j-1)) + (g2%j)
            fromDiag = (m%(i-1, j-1)) + (sim (s1!i) (s2!j))
            candidates = [((i-1, j, GapR $ s1!i), fromTop),
                          ((i, j-1, GapL $ s2!j), fromLeft),
                          ((i-1, j-1, Match (s1!i) (s2!j)), fromDiag)]
