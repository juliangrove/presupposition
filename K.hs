{-# LANGUAGE
    FlexibleContexts,
    InstanceSigs,
    TypeSynonymInstances #-}

module K where

-- $intro
-- This module defines, from 'D', a datatype 'K' for representing presupposition
-- in a continuized format so that one can analyze the semantic contribution of
-- indefinites. We defines from 'K' an instance of the 'PMonad' class of
-- Orchard, Petricek, and Mycroft. Although 'K' constitutes a parameterized
-- monad in the implementation, these give rise to graded monads (in real life).
-- The result is thus a faithful implementation of the framework for
-- presupposition of chapter 3 of Grove 2019.

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import Control.Effect.Parameterised
import Model
import InfoStates
import P
import D

newtype PContT m r f g a = PContT { runPContT :: (a -> m g r) -> m f r }

type K e f a = PContT D InfoState e f a

instance Effect m => PMonad (PContT m r) where
  return :: a -> PContT m r f f a
  return a = PContT $ \k -> k a

  (>>=) :: PContT m r f e a -> (a -> PContT m r e g b) -> PContT m r f g b
  m >>= f = PContT $ \k -> runPContT m $ \x -> runPContT (f x) k

(>>=>) :: Effect m => PContT m r f e a -> (a -> PContT m r e g b) -> PContT m r f g b
m >>=> f = PContT $ \k -> runPContT m $ \x -> runPContT (f x) k

upK :: a -> K e e a
upK a = PContT $ \k -> k a

downK :: K f e (a -> b) -> K e g a -> K f g b
downK u v = u >>=> \f -> v >>=> \x -> upK $ f x

joinK :: K f e (K e g b) -> K f g b
joinK m = m >>=> id

instance Effect m => Functor (PContT m r e f) where
  fmap f m = PContT $ \k -> runPContT m (k . f)

monadicLift :: SeqSplit e f (MonoidPlus e f) => D e a -> K (MonoidPlus e f) f a
monadicLift m = PContT $ \k -> m >>>= k

monadicLower :: K e () InfoState -> D e InfoState
monadicLower m = runPContT m upD

newRegister :: InfoState
newRegister l = Setof [ l ++ [x] | x <- entities ]

a :: (SeqSplit () (MonoidPlus e ()) (MonoidPlus e ()),
      SeqSplit e () (MonoidPlus e ())) =>
     Lift OnePlacePred -> K (MonoidPlus e ()) e (Lift Entity)
a p = PContT $ \k ->
        D $ \i ->
              runD (upD (newRegister >+ (p $ \l -> l !! i)) >@
              (k $ \l -> l !! i)) (succ i)
