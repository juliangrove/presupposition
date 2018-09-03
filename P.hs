{-# LANGUAGE
    TypeFamilies,
    GADTs,
    DataKinds,
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances #-}

module P where

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import Model

-- $intro
-- This module defines a datatype 'P' for representing presupposition and
-- defines from it an instance of the 'Effect' class of Orchard, Petricek, and
-- Mycroft. The result is an implementation of the framework for presupposition
-- of Grove 2019.

-- | We introduce a datatype for heterogeneous sequences. The types of such
-- sequences are the types of the effects. This datatype comes with functions
-- 'head' and 'tail', extracting the head and tail of a sequence, respectively.
data Seq p where
  Nil :: Seq ()
  (:+) :: a -> Seq p -> Seq (a, p)

-- | Sequences can be shown.
instance HelpShowSeq (Seq p) => Show (Seq p) where
  show s = "{" ++ helpShowSeq s

class HelpShowSeq a where
  helpShowSeq :: a -> String

instance Show a => HelpShowSeq (Seq (a, ())) where
  helpShowSeq (a :+ Nil) = show a ++ "}"

instance (Show a, HelpShowSeq (Seq (b, p))) => HelpShowSeq (Seq (a, (b, p))) where
  helpShowSeq (a :+ s) = show a ++ " | " ++ helpShowSeq s

head :: Seq (a, p) -> a
head (a :+ s) = a

tail :: Seq (a, p) -> Seq p
tail (a :+ s) = s

-- | A type family corresponding to a monoid whose addition operation is
-- 'MonoidPlus' and whose unit is '()'.
type family MonoidPlus a b where
  MonoidPlus () a = a
  MonoidPlus (a, p) b = (a, MonoidPlus p b)
  
-- | Concatentation for sequences.
(+:+) :: Seq a -> Seq b -> Seq (MonoidPlus a b)
Nil +:+ s = s
(a :+ s1) +:+ s2 = (a :+ (s1 +:+ s2))

-- | A datatype for Naturals is used at the type level.
data Nat = Zero | Succ Nat deriving Show

-- | Witnesses to the type level Naturals provide parameters for the function
-- 'preAnaph'.
data NatWitness n where
  ZeroW :: NatWitness Zero
  SuccW :: NatWitness n -> NatWitness (Succ n)

-- | We define a datatype for presuppositions. Its first parameter is the
-- effect, and the second is the type of the value.
newtype P e a = P { runP :: Seq e -> Maybe a }

-- | The type-level function to help type the function 'anaph'.
type family Insert a e i where
  Insert a p (NatWitness Zero) = (a, p)
  Insert a (b, p) (NatWitness (Succ n)) = (b, Insert a p (NatWitness n))

-- | A function for inserting terms into sequences, used in the definition of
-- 'anaph'.
insert :: NatWitness n
          -> a
          -> Seq e
          -> Seq (Insert a e (NatWitness n))
insert ZeroW a e = a :+ e
insert (SuccW n) a (b :+ e) = b :+ insert n a e

-- | A function to implement anaphora resolution for the graded monad 'P'.
preAnaph :: NatWitness n
            -> a
            -> P (Insert a e (NatWitness n)) b
            -> P e b
preAnaph i a m = P $ \s -> runP m $ insert i a s

-- | We include a class with a method 'seqSplit' for splitting sequences in a
-- way that depends on the desired type of the result. This trick echoes the
-- method defined by Orchard, Petricek, and Mycroft in their definition of
-- Reader. The main difference is that, here, the effects are given by
-- sequences, whereas their effects are given by sets.
class SeqSplit s t st where
  seqSplit :: Seq st -> (Seq s, Seq t)

instance SeqSplit () () () where
  seqSplit Nil = (Nil, Nil)

instance SeqSplit () (a, p)  (a, p) where
  seqSplit s = (Nil, s)

instance SeqSplit (a, p) () (a, p) where
  seqSplit s = (s, Nil)

instance SeqSplit p1 (b, p2) p3 => SeqSplit (a, p1) (b, p2) (a, p3) where
  seqSplit (a :+ s) = let (s1, s2) = seqSplit s
                   in (a :+ s1, s2)

-- | We make 'P' an instance of the 'Functor' class.
instance Functor (P e) where
  fmap f (P g) = P $ \s -> g s >>>>= \x -> Just $ f x

-- | Let's quickly redefine a '>>>>=' for Maybe, the original bind of which has
-- been hidden.
(>>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>>>= f = Nothing
Just a >>>>= f = f a

-- | We make 'P' an instance of the 'Effect' class of Orchard, Petricek,
-- and Mycroft.
instance Effect P where
  type Inv P p1 p2 = SeqSplit p1 p2 (MonoidPlus p1 p2)
  
  type Unit P = ()
  type Plus P p1 p2 = MonoidPlus p1 p2

  return a = P $ \x -> Just a
  m >>= k = P $ \xy -> let (x, y) = seqSplit xy
                          in runP m x >>>>= \z -> runP (k z) y

-- | The graded monad operator 'upP' is just 'return'.
upP :: a -> P () a
upP = return

-- | The graded monad operator 'downP' is just sequential application.
downP :: (SeqSplit e (MonoidPlus f ()) (MonoidPlus e (MonoidPlus f ())),
          SeqSplit f () (MonoidPlus f ())) =>
         P e (a -> b) -> P f a -> P (MonoidPlus e (MonoidPlus f ())) b
downP u v = u >>= \f -> v >>= \x -> return $ f x

-- | The graded monad operator 'joinP' is just join.
joinP :: SeqSplit f g (MonoidPlus f g) => P f (P g b) -> P (MonoidPlus f g) b
joinP m = m >>= id

-- | The turnstile.
(||-) :: Bool -> a -> Maybe a
True ||- a = Just a
False ||- a = Nothing

-- | Let's define a static meaning for _the_.
theSta :: OnePlacePred -> P (Entity, ()) Entity
theSta = \p -> P $ \s -> p (head s) ||- (head s)
