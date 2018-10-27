{-# LANGUAGE TypeInType, GADTs, ScopedTypeVariables #-}
module SizedList where

-- | A Natural number is either zero, or a successor to another natural number.
data Nat = Zero | Suc Nat

data SizedList a (n :: Nat) where
    Nil :: SizedList a 'Zero
    (:-:) :: a -> SizedList a m -> SizedList a ('Suc m)
-- Make SizedList right associative with the same precedence as normal list consing.
infixr 5 :-:

-- | Gives a nice format for Sized Lists of the form [e1, e2, e3, ...; size].
instance Show a => Show (SizedList a n) where
    show xs = "[" ++ go xs ++ "; " ++ show (len xs) ++ "]"
        where
            go :: SizedList a m -> String
            go Nil = ""
            go (y :-: Nil) = show y
            go (y :-: ys) = show y ++ ", " ++ go ys


-- | Gives the length of a SizedList
-- This could be more type safe with the Singleton pattern but this is outside the scope of my current effort.
len :: SizedList a m -> Int
len Nil = 0
len (_ :-: xs) = 1 + len xs
