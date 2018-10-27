{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables, DeriveFunctor, StandaloneDeriving #-}
module SizedList where

-- | A Natural number is either zero, or a successor to another natural number.
data Nat = One | Suc Nat

data SizedList (n :: Nat) a where
    Last :: a -> SizedList 'One a
    (:-:) :: a -> SizedList m a -> SizedList ('Suc m) a
-- Make SizedList right associative with the same precedence as normal list consing.
infixr 5 :-:

-- | Gives a nice format for Sized Lists of the form [e1, e2, e3, ...; size].
instance Show a => Show (SizedList n a) where
    show xs = "[" ++ go xs ++ "; " ++ show (len xs) ++ "]"
        where
            go :: SizedList m a -> String
            go (Last y) = show y
            go (y :-: ys) = show y ++ ", " ++ go ys

deriving instance Functor (SizedList n)


-- | Gives the length of a SizedList
-- This could be more type safe with the Singleton pattern but this is outside the scope of my current effort.
len :: SizedList m a -> Int
len (Last _) = 1
len (_ :-: xs) = 1 + len xs

sizedZip :: (a -> b -> c) -> SizedList n a -> SizedList n b -> SizedList n c
sizedZip f (Last x) (Last y) = Last (f x y)
sizedZip f (x :-: xs) (y :-: ys) = f x y :-: sizedZip f xs ys

sizedSum :: Num a => SizedList n a -> a
sizedSum (Last x) = x
sizedSum (x :-: xs) = x + sizedSum xs