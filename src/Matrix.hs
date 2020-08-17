{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, StandaloneDeriving, DeriveFunctor #-}
module Matrix where

import SizedList

newtype Matrix (m :: Nat) (n :: Nat) a where
    -- | Matrix is an m x n matrix of elements of type a
    -- The implementation at the moment is that it is a list of n columns of m elements.
    -- For the sake of efficiency this implementation will likely change.
    Matrix :: SizedList n (SizedList m a) -> Matrix m n a

deriving instance Show a => Show (Matrix m n a)
deriving instance Functor (Matrix m n)

type Matrix_ m n a = SizedList n (SizedList m a)

add :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
add (Matrix a) (Matrix b) = Matrix (add' a b)

add' :: Num a => Matrix_ m n a -> Matrix_ m n a -> Matrix_ m n a
add' a b =
  case (a, b) of
      (Last x, Last y) -> Last (addColumn x y)
      (firstA :-: restA, firstB :-: restB) -> addColumn firstA firstB :-: add' restA restB
      -- (_, _) -> error "impossible"
    where
      addColumn :: Num a => SizedList r a -> SizedList r a -> SizedList r a
      -- Notice that Haskell doesn't throw a warning that I might be missing a case
      -- Where one list is a single element (Last x), where the other is not (y :-: ys) like it would
      -- for normal (non-empty) lists. This is because SizedList comes with enough type information to see
      -- that if that were the case, xs and ys would have different types
      -- Contradicting the above type signature.
      -- If I didn't use SizedLists, I would have to add the other two cases to get
      -- rid of any random haskell warnings and just throw in unneeded runtime errors instead.
      addColumn (Last x) (Last y) = Last (x + y)
      addColumn (x :-: xs) (y :-: ys) = (x + y) :-: addColumn xs ys
      -- Haskell even complains these last two cases are redundant.
      -- addColumn _ (Last _) = error "impossible"
      -- addColumn (Last _) _ = error "impossible"


transpose :: Matrix m n a -> Matrix n m a
transpose (Matrix a) = Matrix (transpose' a)

transpose' :: Matrix_ m n a -> Matrix_ n m a
transpose' (Last xs) = fmap Last xs
transpose' (x :-: xs) =
    -- Once the rest of the matrix is transposed we can transpose the first column by taking each element from the leftmost column and
    -- adding it to the top of the transposed right part.
    -- sizedZip makes this easy.
    sizedZip (:-:) x (transpose' xs)
        

mult :: Num a => Matrix m n a -> Matrix n r a -> Matrix m r a
mult (Matrix a) (Matrix b) = Matrix (mult' a b)

dotProd' :: Num a => SizedList n a -> SizedList n a -> a
dotProd' xs ys = sizedSum (sizedZip (*) xs ys)


mult' :: Num a => Matrix_ m n a -> Matrix_ n r a -> Matrix_ m r a
mult' a = columnMult a'
    where
      -- a' and b have the same number of rows.
      -- a' :: Matrix_ n m a
      a' = transpose' a

      columnMult :: Num a => SizedList m (SizedList n a) -> SizedList r (SizedList n a) -> SizedList r (SizedList m a)
      columnMult (Last x) (Last y) = Last . Last $ dotProd' x y
      columnMult (Last x) (y :-: ys) = Last (dotProd' x y) :-: columnMult (Last x) ys
      columnMult (x :-: xs) ys = let first = columnMult (Last x) ys in sizedZip (\(Last c) cs -> c:-:cs) first (columnMult xs ys)
      -- All of the below functions are incorrect and it turns out that Haskell can catch that thhese are wrong thruogh the types.
      -- columnMult (x :-: xs) (Last y) = let Last first = columnMult (Last x) (Last y) in fmap (first :-:) (columnMult xs (Last y))
      -- columnMult (x :-: xs) (Last y) = let first = columnMult (Last x) (Last y) in sizedZip (:-:) first (columnMult xs (Last y))
      -- columnMult (x :-: xs) ys = let first = columnMult (Last x) ys in sizedZip (:-:) first (columnMult (Last x) ys)
      -- columnMult (Last x) (y :-: ys) = let first = columnMult (Last x) (Last y) in sizedZip (:-:) first (columnMult (Last x) ys)
      -- columnMult (x :-: xs) ys = let first = columnMult (Last x) ys in first :-: columnMult xs ys

