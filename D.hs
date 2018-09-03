{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts,
    UndecidableInstances,
    InstanceSigs #-}

module D where

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import Model
import InfoStates
import P

-- $intro
-- This module defines, from 'P', a datatype constructor 'D' for representing
-- presupposition and defines from it an instance of the 'Effect' class of
-- Orchard, Petricek, and Mycroft. The result is an implementation of the
-- framework for presupposition of chapter 3 of Grove 2019.

-- | We define a datatype 'D' for dynamic presupposition. 'D' takes 'P' and
-- implicitly gives it 'ReaderT' functionality so that it is sensitive to the
-- discourse context, as well as 'StateT' functionality so that it is sensitive
-- to an incoming index.
newtype D e a = D { runD :: Int -> (InfoState -> InfoState) -> P e (a, Int) }

-- | We make 'D' an instance of the 'Effect' class of Orchard, Petricek, and
-- Mycroft. Indeed, the effects are the same as those of 'P'.
instance Effect D where
  type Inv D p1 p2 = Inv P p1 p2

  type Unit D = Unit P
  type Plus D p1 p2 = Plus P p1 p2

  return :: a -> D () a
  return a = D $ \i c -> return (a, i)

  (>>=) :: SeqSplit e f (MonoidPlus e f) =>
           D e a -> (a -> D f b) -> D (MonoidPlus e f) b
  m >>= f = D $ \i c -> runD m i c >>= \(v, j) -> runD (f v) j c

-- | For any given effect, 'D' constitutes a 'Functor'.
instance Functor (D e) where
  fmap f m = D $ \i c -> fmap (\(a, j) -> (f a, j)) $ runD m i c

-- | We define a makeshift graded bind so as to provide a non-conflicting
-- identifier for use outside the current module.
(>>>=) :: SeqSplit e f (MonoidPlus e f) => D e a -> (a -> D f b) -> D (Plus D e f) b
(>>>=) = (>>=)

-- | The graded monad operator 'upD' is just 'return'.
upD :: a -> D () a
upD = return

-- | The graded monad operator 'downD' is just sequential application.
downD :: (SeqSplit e (MonoidPlus f ()) (MonoidPlus e (MonoidPlus f ())),
          SeqSplit f () (MonoidPlus f ())) =>
         D e (a -> b) -> D f a -> D (MonoidPlus e (MonoidPlus f ())) b
downD u v = u >>= \f -> v >>= \x -> return $ f x

-- | The graded monad operator 'joinD' is just join.
joinD :: SeqSplit e f (MonoidPlus e f) => D e (D f b) -> D (MonoidPlus e f) b
joinD m = m >>= id

-- | We implement anaphora resolution via a function 'anaph', which takes a
-- position to plug, a term with which to plug it, and the effectual meaning
-- undergoing anaphora.
anaph :: NatWitness n
         -> a
         -> D (Insert (NatWitness n) a e) b
         -> D e b
anaph i a m = D $ \j c -> preAnaph i a $ runD m j c

-- | Let's define a dynamic meaning for _the_.
the :: Lift OnePlacePred -> D (Lift Entity, ()) (Lift Entity)
the = \p -> D $ \i c -> P $ \s -> isTrue (c $ p $ head s) ||- (head s, i)

-- | We define a function '>@' for dynamic discourse update.
(>@) :: (SeqSplit e (MonoidPlus f ()) (MonoidPlus e (MonoidPlus f ())),
         SeqSplit f () (MonoidPlus f ())) =>
        D e InfoState
     -> D f InfoState
     -> D (MonoidPlus e (MonoidPlus f ())) InfoState
phi >@ psi = phi >>= \p -> D $ \i c -> runD psi i (c . (p =>>)) >>=
                       \(q, j) -> return (p >+ q, j)

-- | We can 'reset' meanings by plugging in the identity context 'id' and
-- plugging any reference to an incoming state with '0'.
reset :: D e a -> D e a
reset m = D $ \i c -> fmap (\(v, j) -> (v, i + j)) $ runD m 0 id

-- | We define a function to 'unWrap' a meaning and see what value it might be
-- holding (something in the 'Maybe' monad).
unWrap :: D () a -> Maybe a
unWrap m = fmap fst $ runP (runD m 0 id) Nil

-- | We define a function 'checkForTruth' to check whether or not a trivially
-- effectual information state is 'Just True', 'Just False', or 'Nothing'.
checkForTruth :: D () InfoState -> Maybe Bool
checkForTruth m = fmap (isTrue . (true =>>)) $ unWrap m
