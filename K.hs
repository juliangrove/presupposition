{-# LANGUAGE
    FlexibleContexts,
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

(>>=>) :: PContT m r f e a -> (a -> PContT m r e g b) -> PContT m r f g b
m >>=> f = PContT $ \k -> runPContT m $ \x -> runPContT (f x) k

upK :: a -> K e e a
upK a = PContT $ \k -> k a

downK :: K f e (a -> b) -> K e g a -> K f g b
downK u v = u >>=> \f -> v >>=> \x -> upK $ f x

joinK :: K f e (K e g b) -> K f g b
joinK m = m >>=> id

instance Effect m => Functor (PContT m r e f) where
  fmap f m = PContT $ \k -> runPContT m (k . f)

(<*>) :: Effect m => PContT m r f e (a -> b) -> PContT m r e g a -> PContT m r f g b
u <*> v = u >>=> \f -> v >>=> \x -> PContT $ \k -> k $ f x

type K e f a = PContT D InfoState e f a

monadicLift :: SeqSplit e f (MonoidPlus e f) => D e a -> K (MonoidPlus e f) f a
monadicLift m = PContT $ \k -> m >>>= k

monadicLower :: K e () InfoState -> D e InfoState
monadicLower m = runPContT m upD

newRegister :: InfoState
newRegister l = Setof [ l ++ [x] | x <- entities ]

a :: (SeqSplit () (MonoidPlus e ()) (MonoidPlus e ()),
      SeqSplit e () (MonoidPlus e ())) =>
     LiftedOnePlacePred -> K (MonoidPlus e ()) e LiftedEntity
a p = PContT $ \k ->
        D $ \i ->
              runD (upD (newRegister >+ (p $ \l -> l !! i)) >@
              (k $ \l -> l !! i)) (succ i) 
