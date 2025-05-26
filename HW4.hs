{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror MultiSet.hs HW4.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Num Bool is an orphan instance (not defined where Num or Bool are defined), so we need to silence the warning.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import MultiSet qualified as MS
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))

-- Section 2: JSON data and Jsonable typeclass
newtype JString = JString String deriving (Show, Eq)

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString JString
  | JsonInt Integer
  | JsonDouble Double
  | JsonArray [Json]
  | JsonObject (Map.Map String Json)
  deriving (Show, Eq)

class Jsonable a where
  toJson :: a -> Json
  fromJson :: Json -> Maybe a

instance Jsonable Bool
instance Jsonable JString
instance Jsonable Integer
instance Jsonable Double
instance (Jsonable a, Jsonable b) => Jsonable (a, b)
instance (Jsonable a, Jsonable b, Jsonable c) => Jsonable (a, b, c)
instance Jsonable a => Jsonable (Maybe a)
instance (Jsonable l, Jsonable r) => Jsonable (Either l r)
instance Jsonable a => Jsonable [a]

data Matrix a = Matrix [[a]] deriving (Show, Eq)

instance (Jsonable a, Ord a) => Jsonable (MS.MultiSet a)
instance Jsonable a => Jsonable (Matrix a)

-- A sparse matrix is a more efficient representation of a matrix when most of the entries are zero.
-- Note that zero values should not appear in the map.
data SparseMatrix a
  = SparseMatrix
  { rows :: Integer
  , cols :: Integer
  , entries :: Map.Map (Integer, Integer) a
  }
  deriving (Show, Eq)
instance Jsonable a => Jsonable (SparseMatrix a)

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
instance Jsonable a => Jsonable (Tree a)

-- Section 3: Num
-- Subsection: Num instances
instance Num Bool

data Expression a
  = Iden String
  | Lit Integer
  | Plus (Expression a) (Expression a)
  | Minus (Expression a) (Expression a)
  | Mult (Expression a) (Expression a)
  | Div (Expression a) (Expression a)
  | Signum (Expression a)
  deriving (Eq, Show)
  
instance Num a => Num (Expression a)

newtype MatrixSum a = MatrixSum {getMS :: Matrix a} deriving (Show, Eq)
newtype MatrixMult a = MatrixMult {getMM :: Matrix a} deriving (Show, Eq)
instance Num a => Semigroup (MatrixSum a)
instance Num a => Semigroup (MatrixMult a)

newtype SparseMatrixSum a = SparseMatrixSum {getSMS :: SparseMatrix a} deriving (Show, Eq)
newtype SparseMatrixMult a = SparseMatrixMult {getSMM :: SparseMatrix a} deriving (Show, Eq)

-- These have Eq constraint so you can filter out zero values, which should not appear in sparse matrices.
instance (Num a, Eq a) => Semigroup (SparseMatrixSum a)
instance (Num a, Eq a) => Semigroup (SparseMatrixMult a)

-- Subsection: General functions
evalPoly :: Num a => [a] -> a -> a

type Length = Int
type I = Int
type J = Int
pathsOfLengthK :: Length -> I -> J -> Matrix Int -> Int
hasPath :: I -> J -> Matrix Int -> Bool

-- Section 4: Simplify expressions
-- We constrain the type to Integral so we can use integer division
simplify :: Expression Integer -> Expression Integer
inlineExpressions :: [(Expression Integer, String)] -> [(Expression Integer, String)]
