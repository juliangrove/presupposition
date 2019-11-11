{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    InstanceSigs,
    TypeOperators,
    DataKinds,
    PolyKinds #-}

module D where

import Prelude hiding (Monad(..))
import Control.Effect
import Data.Type.Set ((:++))
import Model
import InfoStates
import P

-- $intro
-- This module defines, from 'P', a datatype constructor 'D' for representing
-- terms with presuppositions within a dynamic semantics and defines from it an
-- instance of the /Effect/ class of Orchard, Petricek, and Mycroft. The result
-- is an implementation of the framework for presupposition of chapter 3 of
-- Grove 2019.

-- | A type constructor to add reader functionality to a graded monad.
newtype GReaderT s m e a = GReaderT { runGReaderT :: s -> m e a }

-- | For any graded monad @m@, 'GReaderT' provides a /Functor/.
instance Functor (m e) => Functor (GReaderT s m e) where
  fmap f m = GReaderT $ \s -> fmap f $ runGReaderT m s

-- | For any graded monad @m@, 'GReaderT' provides an instance of the /Effect/
-- class.
instance (Effect m) => Effect (GReaderT s m) where
  type Inv (GReaderT s m) e f = Inv m e f

  type Unit (GReaderT s m) = Unit m
  type Plus (GReaderT s m) e f = Plus m e f

  return :: a -> GReaderT s m (Unit (GReaderT s m)) a
  return a = GReaderT $ \s -> return a

  (>>=) :: (Inv (GReaderT s m) e f)
        => GReaderT s m e a
        -> (a -> GReaderT s m f b)
        -> GReaderT s m (Plus (GReaderT s m) e f) b
  m >>= k = GReaderT $ \s -> runGReaderT m s >>= \n -> runGReaderT (k n) s

-- | A datatype 'D' for dynamic presupposition. 'D' takes 'P' and implicitly
-- gives it /GReaderT/ functionality so that it is sensitive to the discourse
-- context, as well as /GReaderT/ functionality so that it is sensitive to an
-- incoming index.
newtype D e a = D { runD :: GReaderT Int
                              (GReaderT (InfoState -> InfoState)
                                P) e a }

-- | Given any effect, 'D' is a /Functor/.
instance Functor (D e) where
  fmap f m = D $ fmap f $ runD m
  
-- | 'D' is 'Joinable'.
instance Joinable P e f => Joinable D e f where
  join m = D $ (>>= id) $ fmap runD $ runD m

-- | 'D' is an instance of the /Effect/ class of Orchard, Petricek, and Mycroft.
-- The effects are the same as those of 'P' and 'R'.
instance Effect D where
  type Inv D e f = Joinable D e f

  type Unit D = '[]
  type Plus D e f = e :++ f

  return :: a -> D '[] a
  return a = D $ return a

  (>>=) :: Joinable D e f => D e a -> (a -> D f b) -> D (e :++ f) b
  m >>= f = join $ fmap f m

-- | 'upD' is just @pure@.
upD :: a -> D '[] a
upD a = return a

-- | 'downD' is just @(<*>)@.
downD :: Joinable D e f => D e (a -> b) -> D f a -> D (e :++ f) b
downD u = \v -> join $ fmap (\f -> fmap (\x -> f x) v) u

-- | A makeshift graded bind so as to provide a non-conflicting
-- identifier for use outside the current module.
(>>>=) :: Joinable D e f => D e a -> (a -> D f b) -> D (e :++ f) b
(>>>=) = (>>=)

-- | Anaphora resolution for the graded monad 'D'.
instance Anaph n e P => Anaph n e D where
  anaph n a (D (GReaderT m)) = D $ GReaderT $ \s ->
                                     GReaderT $ \c ->
                                       anaph n a $ (runGReaderT $ m s) c
  
-- | A dynamic meaning for /the/.
the :: Lift (Entity -> Bool) -> D '[Lift Entity] (Lift Entity)
the = \p -> D $ GReaderT $ \i ->
                  GReaderT $ \c ->
                    P $ Comp $ \x -> Val $ isTrue (c $ p x) ||- x

-- | A function '>@' for dynamic discourse update.
(>@) :: Joinable D e f
     => D e InfoState -> D f InfoState -> D (e :++ f) InfoState
phi >@ psi = phi >>= \p -> fmap (\q -> p >+ q) $
                             D $ GReaderT $ \i ->
                               GReaderT $ \c ->
                                 runGReaderT ((runGReaderT . runD) psi i)
                                   (c . (p =>>))

-- | A function to 'unWrap' a meaning and see what value it might be holding
-- (something in the /Maybe/ monad).
unWrap :: D '[] a -> Maybe a
unWrap m = toMaybe $ runR $ runP (runGReaderT ((runGReaderT . runD) m 0) id)

-- | A function 'checkForTruth' to check whether or not a trivially effectual
-- information state is @Just True@, @Just False@, or @Nothing@.
checkForTruth :: D '[] InfoState -> Maybe Bool
checkForTruth m = fmap (isTrue . (true =>>)) $ unWrap m
