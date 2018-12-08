module NeedlemanWunsch where

import Data.Array
import Data.List (maximumBy, intersperse)

data Pair a b = Start
              | GapL b
              | GapR a
              | Match a b

data Alignment a b = Alignment [Pair a b]

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

-- Compute the total alignment score based on the similarity function and gap penalties
alignmentScore :: (a -> b -> Double) -> Double -> Double -> Alignment a b -> Double
alignmentScore sim gapl gapr (Alignment pairs) = sum (map value pairs) where
    value (GapL _) = gapl
    value (GapR _) = gapr
    value (Match x y) = sim x y

-- Pointer to the cell whose value contributed to the current one.
-- In contrast to classical N-W, we have unique pointers.
data Source = Origin
            | FromLeft
            | FromTop
            | FromDiag

-- Type of the N-W matrix entry: pointer to source cell and the value
data Entry = Entry Source Double

src :: Entry -> Source
src (Entry s _) = s

val :: Entry -> Double
val (Entry _ v) = v


-- The Needleman-Wunsch dynamic programming algorithm
align :: [a] -> [b] -> (a -> b -> Double) -> Double -> Double -> Alignment a b
align stream1 stream2 sim gapl gapr = Alignment (walkback (length s1) (length s2) []) where
    -- Convert to 1-based arrays for easy indexing and fast random access
    s1 = listArray (1, length stream1) stream1
    s2 = listArray (1, length stream2) stream2

    -- Fill in the N-W matrix
    fill :: Int -> Int -> Entry
    fill 0 0 = Entry Origin 0.0
    fill 0 j = Entry FromLeft (gapr*(fromIntegral j))
    fill i 0 = Entry FromTop (gapl*(fromIntegral i))
    fill i j = maximumBy maxVal scores where
        scores = [Entry FromLeft ((val $ m!(i, j-1)) + gapl),
                  Entry FromTop ((val $ m!(i-1, j)) + gapr),
                  Entry FromDiag ((val $ m!(i-1, j-1)) + sim (s1!i) (s2!j))]
        maxVal e1 e2 = if val e1 >= val e2 then GT else LT

    -- Walk through the matrix and reconstruct the best alignment
    walkback 0 0 complete = complete
    walkback i j partial = walkback i' j' (pair:partial) where
        (i', j', pair) = case src $ m!(i, j) of
            FromLeft -> (i, j-1, GapL $ s2!j)
            FromTop  -> (i-1, j, GapR $ s1!i)
            FromDiag -> (i-1, j-1, Match (s1!i) (s2!j))

    -- The N-W matrix itself
    m :: Array (Int, Int) Entry
    m = listArray dims [fill i j | (i, j) <- range dims]
    dims = ((0, 0), (length s1, length s2))
