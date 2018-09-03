{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts,
    UndecidableInstances,
    InstanceSigs #-}

module D where

-- $intro
-- This module defines, from 'P', a datatype 'D' for representing presupposition
-- and defines from it an instance of the 'Effect' class of Orchard, Petricek,
-- and Mycroft. The result is an implementation of the framework for
-- presupposition of chapter 3 of Grove 2019.

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import Model
import InfoStates
import P

newtype D e a = D { runD :: Int -> (InfoState -> InfoState) -> P e (a, Int) }

instance Effect D where
  type Inv D p1 p2 = SeqSplit p1 p2 (MonoidPlus p1 p2)

  type Unit D = Unit P
  type Plus D p1 p2 = Plus P p1 p2

  return :: a -> D () a
  return a = D $ \i c -> P $ \s -> True ||- (a, i)

  m >>= f = D $ \i c -> joinP $ fmap (\p -> let (a, j) = p
                                            in runD (f a) j c)
                                     $ runD m i c
instance Functor (D e) where
  fmap f m = D $ \i c -> fmap (\(a, j) -> (f a, j)) $ runD m i c

(>>>=) :: SeqSplit e f (MonoidPlus e f) => D e a -> (a -> D f b) -> D (Plus D e f) b
(>>>=) = (>>=)

upD :: a -> D () a
upD = return

downD :: (SeqSplit e (MonoidPlus f ()) (MonoidPlus e (MonoidPlus f ())),
          SeqSplit f () (MonoidPlus f ())) =>
         D e (a -> b) -> D f a -> D (MonoidPlus e (MonoidPlus f ())) b
downD u v = u >>>= \f -> v >>>= \x -> upD $ f x

joinD :: SeqSplit e f (MonoidPlus e f) => D e (D f b) -> D (MonoidPlus e f) b
joinD m = m >>>= id

anaph :: NatWitness n
         -> a
         -> D (Insert a e (NatWitness n)) b
         -> D e b
anaph i a m = D $ \j c -> preAnaph i a $ runD m j c

the :: Lift OnePlacePred -> D (Lift Entity, ()) (Lift Entity)
the = \p -> D $ \i c -> P $ \s -> isTrue (c $ p $ head s) ||- (head s, i)

(>@) :: (SeqSplit e (MonoidPlus f ()) (MonoidPlus e (MonoidPlus f ())),
         SeqSplit f () (MonoidPlus f ())) =>
        D e InfoState
     -> D f InfoState
     -> D (MonoidPlus e (MonoidPlus f ())) InfoState
phi >@ psi = phi >>= \p -> D $ \i c -> runD psi i (c . (p =>>)) >>=
                       \(q, j) -> return (p >+ q, j)

reset :: D e a -> D e a
reset m = D $ \i c -> fmap (\(v, j) -> (v, i + j))$ runD m 0 id

unWrap :: D () a -> Maybe a
unWrap m = fmap fst $ runP (runD m 0 id) Nil

checkForTruth :: D () InfoState -> Maybe Bool
checkForTruth m = fmap (isTrue . (true =>>)) $ unWrap m
