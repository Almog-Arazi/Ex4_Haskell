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

instance Jsonable Bool where
  toJson b = JsonBool b
  fromJson (JsonBool b) = Just b
  fromJson _ = Nothing

instance Jsonable JString where
  toJson (JString s) = JsonString (JString s)
  fromJson (JsonString (JString s)) = Just (JString s)
  fromJson _ = Nothing

instance Jsonable Integer where
  toJson s = JsonInt s
  fromJson (JsonInt s) = Just s
  fromJson _ = Nothing

instance Jsonable Double where
  toJson s = JsonDouble s
  fromJson (JsonDouble s) = Just s
  fromJson _ = Nothing

instance (Jsonable a, Jsonable b) => Jsonable (a, b) where
  toJson (a, b) = JsonArray [toJson a, toJson b]
  fromJson (JsonArray [a, b]) = case (fromJson a, fromJson b) of
    (Just a, Just b) -> Just (a, b)
    _ -> Nothing
  fromJson _ = Nothing

instance (Jsonable a, Jsonable b, Jsonable c) => Jsonable (a, b, c) where
  toJson (a, b, c) = JsonArray [toJson a, toJson b, toJson c]
  fromJson (JsonArray [a, b, c]) = case (fromJson a, fromJson b, fromJson c) of
    (Just a, Just b, Just c) -> Just (a, b, c)
    _ -> Nothing
  fromJson _ = Nothing

instance Jsonable a => Jsonable (Maybe a) where
  toJson (Just a) = toJson a
  toJson Nothing = JsonNull
  fromJson (JsonNull) = Nothing
  fromJson json = case fromJson json of
    Just a -> Just (Just a)
    Nothing -> Nothing

instance (Jsonable l, Jsonable r) => Jsonable (Either l r) where 
  toJson (Left a) = JsonObject (Map.fromList [("Left", toJson a)])
  toJson (Right a) = JsonObject (Map.fromList [("Right", toJson a)])
  fromJson (JsonObject obj) =
    case Map.lookup "Left" obj of
      Just val ->
        case fromJson val of
          Just leftVal -> Just (Left leftVal)
          Nothing -> Nothing
      Nothing ->
        case Map.lookup "Right" obj of
          Just val ->
            case fromJson val of
              Just rightVal -> Just (Right rightVal)
              Nothing -> Nothing
          Nothing -> Nothing


sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (x:xs) =
  case x of
    Nothing -> Nothing
    Just v ->
      case sequenceMaybe xs of
        Nothing -> Nothing
        Just vs -> Just (v : vs)


instance Jsonable a => Jsonable [a] where
  toJson a = JsonArray (map toJson a)
  fromJson (JsonArray a) = sequenceMaybe (map fromJson a)
  fromJson _ = Nothing

data Matrix a = Matrix [[a]] deriving (Show, Eq)

instance (Jsonable a, Ord a) => Jsonable (MS.MultiSet a) where
  toJson ms = JsonObject (Map.fromList [("MultiSet", toJson (MS.toList ms))])
  fromJson (JsonObject obj) = case Map.lookup "MultiSet" obj of
    Just jsonList -> case fromJson jsonList of
      Just xs -> Just (MS.fromList xs)
      Nothing -> Nothing
    Nothing -> Nothing

instance Jsonable a => Jsonable (Matrix a) where
  toJson (Matrix a) = JsonArray (map (JsonArray . map toJson) a)
  fromJson (JsonArray rows) = 
    case sequenceMaybe (map parseRow rows) of
      Just parsed -> Just (Matrix parsed)
      Nothing -> Nothing
    where
      parseRow (JsonArray elems) = sequenceMaybe (map fromJson elems)
      parseRow _ = Nothing

  fromJson _ = Nothing


-- A sparse matrix is a more efficient representation of a matrix when most of the entries are zero.
-- Note that zero values should not appear in the map.
data SparseMatrix a
  = SparseMatrix
  { rows :: Integer
  , cols :: Integer
  , entries :: Map.Map (Integer, Integer) a
  }
  deriving (Show, Eq)
instance Jsonable a => Jsonable (SparseMatrix a) where 
  toJson (SparseMatrix rows cols entries) =
    JsonObject (Map.fromList [
      ("rows", JsonInt rows),
      ("cols", JsonInt cols),
      ("entries", JsonArray (map encodeEntry (Map.toList entries)))
      ])
    where
      encodeEntry ((row, col), val) =
        JsonObject (Map.fromList [
          ("pos", JsonArray [JsonInt row, JsonInt col]),
          ("val", toJson val)
        ])
  fromJson (JsonObject obj) =
    case (Map.lookup "rows" obj, Map.lookup "cols" obj, Map.lookup "entries" obj) of
      (Just rJson, Just cJson, Just (JsonArray entriesJson)) ->
        buildSparseMatrix rJson cJson entriesJson
      _ -> Nothing
  fromJson _ = Nothing

buildSparseMatrix :: Jsonable a => Json -> Json -> [Json] -> Maybe (SparseMatrix a)
buildSparseMatrix rJson cJson entriesJson =
  case (fromJson rJson, fromJson cJson) of
    (Just r, Just c) ->
      case sequenceMaybe (map decodeEntry entriesJson) of
        Just entries -> Just (SparseMatrix r c (Map.fromList entries))
        Nothing -> Nothing
    _ -> Nothing
  where
    decodeEntry (JsonObject obj) =
      case (Map.lookup "pos" obj, Map.lookup "val" obj) of
        (Just (JsonArray [JsonInt row, JsonInt col]), Just vJson) ->
          case fromJson vJson of
            Just val -> Just ((row, col), val)
            Nothing -> Nothing
        _ -> Nothing
    decodeEntry _ = Nothing
  
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
instance Jsonable a => Jsonable (Tree a) where
  toJson Empty = JsonNull
  toJson (Tree left val right) =
    JsonObject (Map.fromList [
      ("Left", toJson left),
      ("Value", toJson val),
      ("Right", toJson right)
    ])

  fromJson JsonNull = Just Empty
  fromJson (JsonObject obj) = case (
      Map.lookup "Left" obj,
      Map.lookup "Value" obj,
      Map.lookup "Right" obj
    ) of
      (Just l, Just v, Just r) -> case (fromJson l, fromJson v, fromJson r) of
        (Just left, Just val, Just right) -> Just (Tree left val right)
        _ -> Nothing
      _ -> Nothing
  fromJson _ = Nothing


-- Section 3: Num
-- Subsection: Num instances
instance Num Bool where
  fromInteger x   = x `mod` 2 == 1
  (+) = (/=)
  (*) = (&&)
  negate = id
  abs = id
  signum = id  
  x - y = x /= y   

data Expression a
  = Iden String
  | Lit Integer
  | Plus (Expression a) (Expression a)
  | Minus (Expression a) (Expression a)
  | Mult (Expression a) (Expression a)
  | Div (Expression a) (Expression a)
  | Signum (Expression a)
  deriving (Eq, Show)
  
instance Num a => Num (Expression a) where
  negate = Mult (Lit (-1))
  (+) = Plus
  (-) = Minus
  (*) = Mult
  signum x = Signum x
  abs x = Mult x (Signum x)
  fromInteger n = Lit (fromInteger n)

newtype MatrixSum a = MatrixSum {getMS :: Matrix a} deriving (Show, Eq)
newtype MatrixMult a = MatrixMult {getMM :: Matrix a} deriving (Show, Eq)
instance Num a => Semigroup (MatrixSum a) where 
  (<>) x y =
    let
      m1 = getMS x
      m2 = getMS y
    in 
      MatrixSum (zipWith (zipWith (+)) m1 m2)


instance Num a => Semigroup (MatrixMult a) where
  (<>) (MatrixMult (Matrix a)) (MatrixMult (Matrix b)) =
    let
      bT = transpose b  -- Transpose of b for easy column access
      result = [ [ sum $ zipWith (*) row col | col <- bT ] | row <- a ]
    in
      MatrixMult (Matrix result)

      
newtype SparseMatrixSum a = SparseMatrixSum {getSMS :: SparseMatrix a} deriving (Show, Eq)
newtype SparseMatrixMult a = SparseMatrixMult {getSMM :: SparseMatrix a} deriving (Show, Eq)

-- These have Eq constraint so you can filter out zero values, which should not appear in sparse matrices.

instance (Num a, Eq a) => Semigroup (SparseMatrixSum a) where
  (<>) (SparseMatrixSum (SparseMatrix r1 c1 e1)) (SparseMatrixSum (SparseMatrix r2 c2 e2))
    | r1 /= r2 || c1 /= c2 = error "SparseMatrixSum: dimension mismatch"
    | otherwise = SparseMatrixSum (SparseMatrix r1 c1 entriesSum)
    where
      merged = Map.unionWith (+) e1 e2
      entriesSum = Map.filter (/= 0) merged

instance (Num a, Eq a) => Semigroup (SparseMatrixMult a) where
  (<>) (SparseMatrixMult (SparseMatrix r1 c1 e1)) (SparseMatrixMult (SparseMatrix r2 c2 e2))
    | c1 /= r2 = error "SparseMatrixMult: dimension mismatch"
    | otherwise = SparseMatrixMult (SparseMatrix r1 c2 result)
    where
      -- for each position (i, k) from e1 and (k, j) from e2:
      candidates =
        [ ((i, j), v1 * v2)
        | ((i, k1), v1) <- Map.toList e1
        , ((k2, j), v2) <- Map.toList e2
        , k1 == k2
        ]
      result = Map.filter (/= 0) $ Map.fromListWith (+) candidates

-- Subsection: General functions
evalPoly :: Num a => [a] -> a -> a
evalPoly coeffs x = sum $ zipWith (\a i -> a * (x ^ i)) coeffs [0..]

type Length = Int
type I = Int
type J = Int
pathsOfLengthK :: Length -> I -> J -> Matrix Int -> Int
pathsOfLengthK k i j m =
  let Matrix m' = getMM (foldr1 (<>) (replicate k (MatrixMult m)))
  in (m' !! i) !! j


hasPath :: I -> J -> Matrix Int -> Bool
hasPath i j (Matrix rows) =
  let n = length rows
      powers = scanl1 (<>) (replicate (n - 1) (MatrixMult (Matrix rows)))
  in any (\(Matrix m) -> (m !! i) !! j > 0) (map getMM powers)


-- Section 4: Simplify expressions
-- We constrain the type to Integral so we can use integer division
simplify :: Expression Integer -> Expression Integer
simplify expr = case expr of
  -- בסיסיים
  Lit n -> Lit n
  Iden x -> Iden x

  -- חיבור
  Plus e1 e2 ->
    let s1 = simplify e1
        s2 = simplify e2
    in case (s1, s2) of
      (Lit 0, e) -> e
      (e, Lit 0) -> e
      (Lit n1, Lit n2) -> Lit (n1 + n2)
      _ -> Plus s1 s2

  -- חיסור
  Minus e1 e2 ->
    let s1 = simplify e1
        s2 = simplify e2
    in case (s1, s2) of
      (e, Lit 0) -> e
      (Lit n1, Lit n2) -> Lit (n1 - n2)
      _ -> Minus s1 s2

  -- כפל
  Mult e1 e2 ->
    let s1 = simplify e1
        s2 = simplify e2
    in case (s1, s2) of
      (Lit 1, e) -> e
      (e, Lit 1) -> e
      (Lit n1, Lit n2) -> Lit (n1 * n2)
      _ -> Mult s1 s2

  -- חילוק
  Div e1 e2 ->
    let s1 = simplify e1
        s2 = simplify e2
    in case (s1, s2) of
      (_, Lit 1) -> s1
      (Lit n1, Lit n2) | n2 /= 0 -> Lit (n1 `div` n2)
      _ -> Div s1 s2

  -- Signum
  Signum e ->
    let s = simplify e
    in case s of
      Lit n -> Lit (signum n)
      Mult e1 e2 -> simplify (Mult (Signum e1) (Signum e2))
      Div e1 e2 -> simplify (Div (Signum e1) (Signum e2))
      _ -> Signum s


      
inlineExpressions :: [(Expression Integer, String)] -> [(Expression Integer, String)]
