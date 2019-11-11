{-# LANGUAGE
    MonadComprehensions,
    TypeFamilies,
    FlexibleInstances #-}

module InfoStates where

import Model

-- $intro
-- This module defines a type for information states, along with various
-- operations on them, including update ('>+') and dynamic implication
-- ('=>>').

-- | A type of sets so that we may construct sets of partial assignments (i.e.,
-- lists of 'entities').
newtype Set a = Setof { getList :: [a] }

-- | Sets can be equated.
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

-- | Sets can be shown.
instance (Show a, Eq a) => Show (Set a) where
  show s = "{" ++ (helpShowSet $ getList $ compact s)

instance Foldable Set where
  foldr f a s = foldr f a $ getList s

-- | 'Set' is an instance of the /Functor/ class.
instance Functor Set where
  fmap f s = Setof $ map f $ getList s

-- | 'Set' is an instance of the /Applicative/.
instance Applicative Set where
  pure a = Setof [a]
  u <*> v = Setof $ getList u <*> getList v

-- | 'Set' is an instance of the /Monad/ class.
instance Monad Set where
  return = pure
  m >>= f = Setof $ getList m >>= \x -> getList $ f x

-- | A predicate 'isEmpty' to check whether or not a set is empty.
isEmpty :: Eq a => Set a -> Bool
isEmpty s = s == Setof []

-- | A type 'InfoState', inhabited by information states. These take partial
-- assignments (i.e., lists of 'entities') and return sets of such.
type InfoState = [Entity] -> Set [Entity]

-- | A type 'LiftedEntity', inhabited by functions from partial assignments
-- (i.e., lists of 'entities') to 'entities'.
type LiftedEntity = [Entity] -> Entity

-- | An information state 'true', which returns the singleton set of any partial
-- assignment (i.e., list of 'entities') it is fed.
true :: InfoState
true a = Setof [a]

-- | An information state 'false', which returns the empty set of partial
-- assignments (i.e., lists of 'entities'), no matter which assignment it is
-- fed.
false :: InfoState
false a = Setof []

-- | A connective 'neg' for negating information states.
neg :: InfoState -> InfoState
neg phi = phi =>> false

-- | A predicate on information states which checks whether or not they are
-- 'true' (by running them on the empty partial assignment).
isTrue :: InfoState -> Bool
isTrue iState = iState [] == true []

-- | Information state update.
(>+)  :: InfoState -> InfoState -> InfoState
p >+ q = \s -> [ u | t <- p s, u <- q t ]

-- | Dynamic implication.
(=>>) :: InfoState -> InfoState -> InfoState
p =>> q = \s -> Setof [ s | and $ fmap (\l -> not $ isEmpty $ q l) $ p s ]

-- | A type family 'Lift' for lifting arbitrary types into their information
-- state variants.
type family Lift a where
  Lift Bool = InfoState
  Lift Entity = LiftedEntity
  Lift (a -> b) = Lift a -> Lift b

-- | A class 'LiftPred' with a single method 'lift' for lifting predicates into
-- their information state variants.
class LiftPred a where
  lift :: ([Entity] -> a) -> Lift a

instance LiftPred Bool where
  lift a = \l -> Setof [ l | a l ]

instance LiftPred Entity where
  lift e = e

instance LiftPred a => LiftPred (Entity -> a) where
  lift r = \x -> lift (\l -> r l (x l))

-- | From 'lift', we can define a function 'dyn' which lifts predicates of any
-- arity into their dynamic correspondents.
dyn :: LiftPred a => a -> Lift a
dyn r = lift $ \l -> r
