{-# LANGUAGE
    TypeFamilies,
    GADTs,
    DataKinds,
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts,
    InstanceSigs,
    TypeOperators,
    RankNTypes,
    PolyKinds #-}

module P where

import Prelude hiding (Monad(..))
import Data.Type.Set ((:++))
import Control.Effect
import Model

-- $intro
-- This module defines a datatype constructor 'P' for representing
-- presupposition and defines from it an instance of the /Effect/ class of
-- Orchard, Petricek, and Mycroft. The result is an implementation of the
-- framework for presupposition of chapter 2 of Grove 2019.

-- | A datatype for possible exceptions.
data Exception = Fail

-- | A type constructor 'E' to endow value types with the capability of raising
-- exceptions.
newtype E a = E { runE :: forall o . (a -> o) -> (Exception -> o) -> o }

-- | The 'Joinable' class defines a 'join' operator to be used for Effect
-- monads.
class Joinable m e f where
  join :: m e (m f a) -> m (e :++ f) a

-- | 'E' is a /Functor/.
instance Functor E where
  fmap f0 m = E $ \s f -> runE m (s . f0) f

-- | The datatype 'R' (for "Reader") has either values 'Val' or computations
-- 'Comp', the latter providing a representation of anaphoric dependencies as
-- dependencies on the environment.
data R e a where
  Val :: a -> R '[] a
  Comp :: (t -> R e a) -> R (t ': e) a

-- | 'RunR' exposes either the value or the argument of the function inhabiting
-- 'R'.
type family RunR e a where
  RunR '[] a = a
  RunR (t ': e) a = t -> R e a

-- | Pop the first dependency off of an inhabitant of 'R', or expose its value.
runR :: R e a -> RunR e a
runR (Val v) = v
runR (Comp c) = c

-- | 'R' is a /Functor/.
instance Functor (R e) where
  fmap f (Val v) = Val $ f v
  fmap f (Comp c) = Comp $ \x -> fmap f $ c x

-- | Lift a binary operation on values into its effect-sensitive variant.
liftOp :: (Functor (m e), Functor (m f), Joinable m e f) =>
          (a -> b -> c) -> m (e :: [*]) a -> m (f :: [*]) b -> m (e :++ f) c
liftOp op = \m n -> join $ fmap (\a -> fmap (\b -> op a b) n) m

-- | 'R' is 'Joinable'.
instance Joinable R '[] f where
  join (Val m) = m

-- | 'R' is 'Joinable'.
instance Joinable R e f => Joinable R (t ': e) f where
  join (Comp c) = Comp $ \x -> join $ c x

-- | 'R' is an instance of the /Effect/ class of Orchard, Petricek, and Mycroft.
instance Effect R where
  type Inv R e f = Joinable R e f
  
  type Unit R = '[]
  type Plus R e f = e :++ f

  return :: a -> R '[] a
  return a = Val a

  (>>=) :: Joinable R e f => R e a -> (a -> R f b) -> R (e :++ f) b
  m >>= f = join $ fmap f m

-- | The datatype 'P' endows 'R' with the functionality of 'E'.
newtype P e a = P { runP :: R e (E a) }

-- | We define a class 'Except', in order to assist in definition of 'Joinable'
-- instance for 'P'.
class Except e where
  except :: Exception -> P e a

instance Except '[] where
  except = \x -> P $ Val $ E $ \s f -> f x

instance Except f => Except (t ': f) where
  except = \x -> P $ Comp $ \y -> runP $ except x

-- | 'P' is 'Joinable'.
instance Except f => Joinable P '[] f where
  join (P (Val (E m))) = m id except

-- | 'P' is 'Joinable'.
instance Joinable P e f => Joinable P (t ': e) f where
  join (P (Comp m)) = P $ Comp $ \x -> runP $ join $ P $ m x

-- | 'P' is a /Functor/.
instance Functor (P e) where
  fmap f m = P $ (fmap . fmap) f $ runP $ m

-- | 'P' is an instance of the /Effect/ class of Orchard, Petricek, and Mycroft.
-- The effects are the same as those of 'R'.
instance Effect P where
  type Inv P e f = Joinable P e f

  type Unit P = '[]
  type Plus P e f = e :++ f

  return :: a -> P '[] a
  return a = P $ Val $ E $ \s f -> s a

  (>>=) :: Joinable P e f => P e a -> (a -> P f b) -> P (e :++ f) b
  m >>= k = join $ fmap k m


-- | 'upP' is just @pure@.
upP :: a -> P '[] a
upP a = return a

-- | 'downP' is just @(<*>)@.
downP :: Joinable P e f => P e (a -> b) -> P f a -> P (e :++ f) b
downP u = \v -> join $ fmap (\f -> fmap (\x -> f x) v) u

-- | A datatype for naturals for use at the type level.
data Nat = Zero | Succ Nat

-- | We define singleton types corresponding to the type-level naturals.
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- | The type function 'Insert' inserts a type into a list. 
type family Insert n a e where
  Insert 'Zero t e = t ': e
  Insert ('Succ n) s (t ': e) = t ': Insert n s e

-- | The class 'Anaph' is defined to accomplish anaphora resolution!
class Anaph n e m where
  anaph :: SNat n -> a -> m (Insert n a e) b -> m e b

instance Anaph 'Zero e R where
  anaph SZero a (Comp c) = c a

instance Anaph n e R => Anaph ('Succ n) (b ': e) R where
  anaph (SSucc n) a (Comp c) = Comp $ \x -> anaph n a $ c x

instance Anaph n e R => Anaph n e P where
  anaph n a (P m) = P $ anaph n a m

-- | The turnstile.
(||-) :: Bool -> a -> E a
True ||- a = E $ \s f -> s a
False ||- a = E $ \s f -> f Fail

-- | Return @Just a@ if @m@ returns @a@; return @Nothing@ if @m@ returns an
-- exception.
toMaybe :: E a -> Maybe a
toMaybe m = runE m Just (const Nothing)

-- | Let's define a static meaning for /the/.
theSta :: (Entity -> Bool) -> P '[Entity] Entity
theSta = \p -> P $ Comp $ \x -> Val $ p x ||- x
