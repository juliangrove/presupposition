{-# LANGUAGE
    TypeFamilies,
    GADTs,
    DataKinds,
    EmptyDataDecls,
    MultiParamTypeClasses,
    AllowAmbiguousTypes,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    MonadComprehensions #-}

module InfoStates where

import Model

newtype Set a = Setof { getList :: [a] }

instance Eq a => Eq (Set a) where
  s1 == s2 = let allin = \y z -> and $ map (\x -> elem x $ getList y)
                                     $ getList z
             in allin s1 s2 && allin s2 s1

addToSet :: a -> Set a -> Set a
addToSet a (Setof l) = Setof $ a:l

compact :: Eq a => Set a -> Set a
compact (Setof []) = Setof []
compact (Setof (x:xs)) = case elem x xs of
                           True -> compact $ Setof xs
                           False -> addToSet x $ compact $ Setof xs

helpShowSet :: Show a => [a] -> String
helpShowSet [] = "}"
helpShowSet [a] = show a ++ "}"
helpShowSet (x:xs) = show x ++ ", " ++ helpShowSet xs

instance (Show a, Eq a) => Show (Set a) where
  show s = "{" ++ (helpShowSet $ getList $ compact s)

instance Foldable Set where
  foldr f a s = foldr f a $ getList s

instance Functor Set where
  fmap f s = Setof $ map f $ getList s

instance Applicative Set where
  pure a = Setof [a]
  u <*> v = Setof $ getList u <*> getList v

instance Monad Set where
  return = pure
  m >>= f = Setof $ getList m >>= \x -> getList $ f x

isEmpty :: Eq a => Set a -> Bool
isEmpty s = s == Setof []

type InfoState = [Entity] -> Set [Entity]

true :: InfoState
true a = Setof [a]

isTrue :: InfoState -> Bool
isTrue iState = iState [] == true []

(>+)  :: InfoState -> InfoState -> InfoState
p >+ q = \s -> [ u | t <- p s, u <- q t ]

(=>>) :: InfoState -> InfoState -> InfoState
p =>> q = \s -> Setof [ s | and $ fmap (\l -> not $ isEmpty $ q l) $ p s ]
