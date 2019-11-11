{-# LANGUAGE
    FlexibleContexts,
    InstanceSigs,
    TypeFamilies,
    TypeOperators,
    DataKinds #-}

module K where

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Parameterised
import Data.Type.Set ((:++))
import Model
import InfoStates
import P
import D

-- $intro
-- This module defines, from 'D', a datatype constructor 'K' for representing
-- terms with presuppositions dynamically and in a continuized format so that
-- one can analyze the semantic contribution of indefinites. We define from 'K'
-- an instance of the /PMonad/ class of Orchard Petricek, and Mycroft. Although
-- 'K' constitutes a parameterized monad in the implementation, these gives rise
-- to graded monads (in real life), given a monoid over the parameters. The
-- result is thus a faithful implementation of the framework for presupposition
-- of chapter 3 of Grove 2019.

-- | The datatype 'K' transforms 'D' into its continuized variant.
newtype K e f a = K { runK :: (a -> D f InfoState) -> D e InfoState }

-- | 'K' is an instance of the /PMonad/ class of Orchard, Petricek, and Mycroft.
instance PMonad K where
  return :: a -> K e e a
  return a = K $ \k -> k a

  (>>=) :: K f e a -> (a -> K e g b) -> K f g b
  m >>= f = K $ \k -> runK m $ \x -> runK (f x) k

-- | A makeshift parameterized bind to avoid naming conflicts outside
-- the current module.
(>>=>) :: K f e a -> (a -> K e g b) -> K f g b
m >>=> f = K $ \k -> runK m $ \x -> runK (f x) k

-- | The parameterized monad operator 'upK' is just @pure@.
upK :: a -> K e e a
upK a = K $ \k -> k a

-- | The parameterized monad operator 'downK' is just @(<*>)@.
downK :: K f e (a -> b) -> K e g a -> K f g b
downK u = \v -> joinK $ fmap (\f -> fmap (\x -> f x) v) u

-- | The parameterized monad operator 'joinK' is just @join@.
joinK :: K f e (K e g b) -> K f g b
joinK m = m >>=> id

-- | For any two chosen parameters, 'K' is an instance of the /Functor/ class.
instance Functor (K e f) where
  fmap f m = K $ \k -> runK m (k . f)

-- | Lift a binary operation on values into its K-sensitive variant.
liftOpK :: (a -> b -> c) -> K e f a -> K f g b -> K e g c
liftOpK op = \m n -> joinK $ fmap (\a -> fmap (\b -> op a b) n) m

-- | A 'monadicLift' to transform objects in 'D' to objects in 'K'.
monadicLift :: Joinable D e f => D e a -> K (e :++ f) f a
monadicLift m = K $ \k -> m >>>= k

-- | A 'monadicLower' to transform information states in 'K' to information
-- states in 'D'.
monadicLower :: K e '[] InfoState -> D e InfoState
monadicLower m = runK m upD

-- | A function for generating a 'newRegister' for use in the semantics of 'a'.
newRegister :: InfoState
newRegister l = Setof [ l ++ [x] | x <- entities ]

-- | A function 'a' which provides the meaning of the indefinite article.
a :: Joinable D '[] e  => Lift (Entity -> Bool) -> K e e (Lift Entity)
a p = K $ \k ->
        D $ GReaderT $ \i ->
          (runGReaderT . runD) (upD (newRegister >+ (p $ \l -> l !! i)) >@
            (k $ \l -> l !! i)) (succ i)

-- | An allomorph of 'a' for when it is phonotactically necessary.
an :: Joinable D '[] e => Lift (Entity -> Bool) -> K e e (Lift Entity)
an = a
