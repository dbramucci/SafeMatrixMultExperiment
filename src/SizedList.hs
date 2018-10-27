{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables #-}
module SizedList where

-- | A Natural number is either zero, or a successor to another natural number.
data Nat = Zero | Suc Nat

data SizedList a (n :: Nat) where
    Nil :: SizedList a Zero
    (:-:) :: a -> SizedList a m -> SizedList a (Suc m)

-- | Gives a nice format for Sized Lists of the form [e1, e2, e3, ...; size].
instance Show a => Show (SizedList a n) where
    show xs = "[" ++ go xs ++ "; " ++ show (len xs) ++ "]"
        where
            go :: SizedList a m -> String
            go Nil = ""
            go (x :-: Nil) = show x
            go (x :-: xs) = show x ++ ", " ++ go xs


-- | Gives the length of a SizedList
-- This could be more type safe with the Singleton pattern but this is outside the scope of my current effort.
len :: SizedList a m -> Int
len Nil = 0
len (x :-: xs) = 1 + len xs
