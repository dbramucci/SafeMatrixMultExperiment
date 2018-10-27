{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, StandaloneDeriving #-}
module Matrix where

import SizedList

data Matrix (m :: Nat) (n :: Nat) a where
    -- | Matrix is an m x n matrix of elements of type a
    -- The implementation at the moment is that it is a list of n columns of m elements.
    -- For the sake of efficiency this implementation will likely change.
    Matrix :: SizedList n (SizedList m a) -> Matrix m n a

deriving instance Show a => Show (Matrix m n a)

add :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
-- add (Matrix (Last x)) (Matrix (Last y)) = Matrix (Last (addColumn x y))
-- add (Matrix (firstA :-: restA)) (Matrix (firstB :-: restB)) = Matrix (addColumn firstA firstB :-: restAdded)
add (Matrix columnsA) (Matrix columnsB) =
  case (columnsA, columnsB) of
      (Last x, Last y) -> Matrix (Last (addColumn x y))
      (firstA :-: restA, firstB :-: restB) -> let Matrix restAdded = add (Matrix restA) (Matrix restB) in Matrix (addColumn firstA firstB :-: restAdded)
      (_, _) -> error "impossible"
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
      addColumn _ (Last _) = error "impossible"
      addColumn (Last _) _ = error "impossible"


transpose :: Matrix m n a -> Matrix n m a
transpose (Matrix columns) = Matrix (newRows columns)
        where
          newRows :: SizedList n (SizedList m a) -> SizedList m (SizedList n a)
          newRows (Last xs) = fmap Last xs
          -- Once the rest of the matrix is transposed we can transpose the first column by taking each element from the leftmost column and
          -- adding it to the top of the transposed right part.
          -- sizedZip makes this easy.
          newRows (x :-: xs) = sizedZip (:-:) x restTransposed
            where
              restTransposed = newRows xs

mult :: Num a => Matrix m n a -> Matrix n r a -> Matrix m r a
mult a b = let Matrix aC = a'
               Matrix bC = b
           in Matrix (columnMult aC bC)
    where
      -- a' and b have the same number of rows.
      -- a' :: Matrix n m a
      a' = transpose a

      columnMult :: Num a => SizedList m (SizedList n a) -> SizedList r (SizedList n a) -> SizedList r (SizedList m a)
      columnMult (Last x) (Last y) = Last . Last . sizedSum $ sizedZip (*) x y
      columnMult (Last x) (y :-: ys) = let Last first = columnMult (Last x) (Last y) in first :-: columnMult (Last x) ys
      columnMult (x :-: xs) ys = let first = columnMult (Last x) ys in sizedZip (\(Last c) cs -> c:-:cs) first (columnMult xs ys)
      -- All of the below functions are incorrect and it turns out that Haskell can catch that thhese are wrong thruogh the types.
      -- columnMult (x :-: xs) (Last y) = let Last first = columnMult (Last x) (Last y) in fmap (first :-:) (columnMult xs (Last y))
      -- columnMult (x :-: xs) (Last y) = let first = columnMult (Last x) (Last y) in sizedZip (:-:) first (columnMult xs (Last y))
      -- columnMult (x :-: xs) ys = let first = columnMult (Last x) ys in sizedZip (:-:) first (columnMult (Last x) ys)
      -- columnMult (Last x) (y :-: ys) = let first = columnMult (Last x) (Last y) in sizedZip (:-:) first (columnMult (Last x) ys)
      -- columnMult (x :-: xs) ys = let first = columnMult (Last x) ys in first :-: columnMult xs ys

