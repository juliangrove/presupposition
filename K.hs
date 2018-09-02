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
    InstanceSigs #-}

module K where

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import Control.Effect.Parameterised
import P
import D
import Model
import InfoStates

newtype PContT m r f g a = PContT { runPContT :: (a -> m g r) -> m f r }

instance Effect m => PMonad (PContT m r) where
  return :: a -> PContT m r f f a
  return a = PContT $ \k -> k a

  (>>=) :: PContT m r f e a -> (a -> PContT m r e g b) -> PContT m r f g b
  m >>= f = PContT $ \k -> runPContT m $ \x -> runPContT (f x) k

returnK :: a -> K e e a
returnK a = PContT $ \k -> k a

(>>=>) :: PContT m r f e a -> (a -> PContT m r e g b) -> PContT m r f g b
m >>=> f = PContT $ \k -> runPContT m $ \x -> runPContT (f x) k

instance Effect m => Functor (PContT m r e f) where
  fmap f m = PContT $ \k -> runPContT m (k . f)

(<*>) :: Effect m => PContT m r f e (a -> b) -> PContT m r e g a -> PContT m r f g b
u <*> v = u >>=> \f -> v >>=> \x -> PContT $ \k -> k $ f x

appK :: Effect m => PContT m r f e (a -> b) -> PContT m r e g a -> PContT m r f g b
appK u v = u >>=> \f -> v >>=> \x -> PContT $ \k -> k $ f x

type K e f a = PContT D InfoState e f a

monadicLift :: SeqSplit e f (MonoidPlus e f) => D e a -> K (MonoidPlus e f) f a
monadicLift m = PContT $ \k -> m >>>= k

monadicLower :: K e () InfoState -> D e InfoState
monadicLower m = runPContT m returnDyn

newRegister :: InfoState
newRegister l = Setof [ l ++ [x] | x <- entities ]

a :: (SeqSplit e () (MonoidPlus e ())) =>
     LiftedOnePlacePred -> K (MonoidPlus e ()) e LiftedEntity
a p = PContT $ \k ->
        D $ \i ->
              runD (newRegister >+ (p $ \l -> l !! i) >@
              (k $ \l -> l !! i)) (succ i) 
