{-# LANGUAGE
    TypeFamilies,
    GADTs,
    DataKinds,
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts,
    AllowAmbiguousTypes,
    RankNTypes,
    ExplicitForAll,
    UndecidableInstances,
    InstanceSigs,
    TemplateHaskell #-}

module D where

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import P
import Model
import InfoStates

newtype D e a = D { runD :: Int -> (InfoState -> InfoState) -> P e (a, Int) }

instance Effect D where
  type Inv D p1 p2 = SeqSplit p1 p2 (MonoidPlus p1 p2)

  type Unit D = Unit P
  type Plus D p1 p2 = Plus P p1 p2

  return :: a -> D () a
  return a = D $ \i c -> P $ \s -> True ||- (a, i)

  m >>= f = D $ \i c -> join $ fmap (\p -> let (a, j) = p
                                                 in runD (f a) j c)
                                    $ runD m i c
instance Functor (D e) where
  fmap f m = D $ \i c -> fmap (\(a, j) -> (f a, j)) $ runD m i c

(>>>=) :: SeqSplit e f (MonoidPlus e f) => D e a -> (a -> D f b) -> D (Plus D e f) b
(>>>=) = (>>=)

returnDyn :: a -> D () a
returnDyn = return

type LiftedEntity = [Entity] -> Entity
type LiftedOnePlacePred = LiftedEntity -> InfoState
type LiftedTwoPlacePred = LiftedEntity -> LiftedEntity -> InfoState

the :: LiftedOnePlacePred -> D (LiftedEntity, ()) LiftedEntity
the = \p -> D $ \i c -> P $ \s -> isTrue (c $ p $ head s) ||- (head s, i)

liftOnePlacePred :: OnePlacePred -> LiftedOnePlacePred
liftOnePlacePred p = \x l -> Setof [ l | p $ x l ]

liftTwoPlacePred :: TwoPlacePred -> LiftedTwoPlacePred
liftTwoPlacePred p = \x y l -> Setof [ l | p (x l) (y l) ]

(>@) :: (SeqSplit e () (MonoidPlus e ())) =>
        InfoState -> D e InfoState -> D (MonoidPlus e ()) InfoState
p >@ phi = D $ \i c -> runD phi i (c . (p =>>)) >>=
                       \(q, j) -> return (p >+ q, j)

reset :: D e a -> D e a
reset m = D $ \i c -> fmap (\(v, j) -> (v, i + j))$ runD m 0 id

unWrap :: D () a -> Maybe a
unWrap m = fmap fst $ runP (runD m 0 id) Nil

checkForTruth :: D () InfoState -> Maybe Bool
checkForTruth m = fmap (isTrue . (true =>>)) $ unWrap m
