{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MultiSet (MultiSet, empty, member, count, remove, insert, fromList, toList) where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import Data.Semigroup (Arg (..))
import Data.Set qualified as Set
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))

newtype MultiSet a = MultiSet {_getMultiset :: Set.Set (Arg a Int)}


-- | helper function
lookupS :: Ord a => a -> Set.Set (Arg a Int) -> Maybe (Arg a Int)
lookupS x s =
  case Set.lookupGE (Arg x 0) s of
    Just arg@(Arg y _) | x == y -> Just arg
    _                           -> Nothing


empty :: MultiSet a
empty = MultiSet Set.empty

member :: Ord a => a -> MultiSet a -> Bool
member x (MultiSet s) =
  case lookupS x s of
    Just _  -> True
    Nothing -> False

-- | Returns the count of an element in the multiset, 0 if not present.
count :: Ord a => a -> MultiSet a -> Int
count x (MultiSet s) =
  case lookupS x s of
    Just (Arg _ n) -> n
    Nothing        -> 0

-- | Insert one occurrence of an element into the multiset.
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet s) =
  case lookupS x s of
    Just (Arg _ n) ->
      MultiSet (Set.insert (Arg x (n + 1)) (Set.delete (Arg x n) s))
    Nothing ->
      MultiSet (Set.insert (Arg x 1) s)

-- | Remove one occurrence of an element from the multiset.
remove :: Ord a => a -> MultiSet a -> MultiSet a
remove x (MultiSet s) =
  case lookupS x s of
    Just (Arg _ n)
      | n == 1    -> MultiSet (Set.delete (Arg x n) s)
      | otherwise -> MultiSet (Set.insert (Arg x (n - 1)) (Set.delete (Arg x n) s))
    Nothing -> MultiSet s

-- | Convert a list into a multiset.
fromList :: Ord a => [a] -> MultiSet a
fromList xs = foldl' (\ms x -> insert x ms) empty xs

-- | Convert a multiset into a list, including duplicates.
toList :: Ord a => MultiSet a -> [a]
toList (MultiSet s) = foldr (++) [] (map (\(Arg x n) -> replicate n x) (Set.toList s))

instance Eq a => Eq (MultiSet a) where
  (==) :: Eq a => MultiSet a -> MultiSet a -> Bool
  (MultiSet s1) == (MultiSet s2) = Set.toList s1 == Set.toList s2

instance Show a => Show (MultiSet a)  where
  show (MultiSet s) =
    "{" ++ intercalate " , " (showElements (Set.toList s)) ++ "}"

-- helper
showElements :: Show a => [Arg a Int] -> [String]
showElements [] = []
showElements (Arg x n : rest) =
  replicate n (show x) ++ showElements rest

instance Ord a => Semigroup (MultiSet a) where
  (<>) m1 m2 = fromList (toList m1 ++ toList m2)


instance Ord a => Monoid (MultiSet a) where
  mempty = empty



-- >>> fromList [1,1,2]
-- {1 , 1 , 2}
-- >>> count 1 (fromList [1,1,2])
-- 2
-- >>> count 3 (fromList [1,1,2])
-- 0

-- >>> member 2 (fromList [1,1,2])
-- True

-- >>> member 3 (fromList [1,1,2])
-- False
-- >>> toList (fromList [1,1,2])
-- [1,1,2]
-- >>> show (insert 3 (fromList [1,1]))
-- "{1 , 1 , 3}"
-- >>> show (remove 1 (fromList [1,1,2]))
-- "{1 , 2}"
-- >>> fromList [1,1,2] == fromList [2,1,1]
-- True
-- >>> fromList [1,1,2] == fromList [1,2]
-- True
