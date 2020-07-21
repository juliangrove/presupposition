{-# LANGUAGE
    FlexibleContexts,
    InstanceSigs,
    TypeOperators,
    DataKinds,
    TypeFamilies #-}

module Q where

import Prelude hiding (Monad(..), (<*>), head, tail)
import Control.Effect
import Control.Effect.Parameterised
import Data.Type.Set ((:++))
import Model
import InfoStates
import P
import D
import K

-- $ intro
-- This module defines, from 'K', a datatype constructor 'Q' for representing
-- terms with presuppositions dynamically and in a continuized format so that
-- one can analyze the semantic contribution of non-exceptionally-scoping
-- quantifiers, like /every/. The strategy we adopt to analyze such quantifiers
-- follows that of Charlow 2014, which considers the monad for handling the
-- exceptionally scoping behavior of indefinites to be the "underlying monad"
-- of the continuation monad for handling the behavior of other quantifiers.
-- Here, we take the underlying monad to be 'K' and the continuation monad to be
-- 'Q'. We define from 'Q' an instance of the /PMonad/ class of Petricek,
-- Orchard, and Mycroft.

-- | A datatype constructor 'Q' that transforms 'K' monad into its continuized
-- variant as another parameterized monad. The extra parameter h is the
-- incoming parameter of the continuation, which remains the incoming parameter
-- of the result.
newtype Q h f g a = Q { runQ :: (a -> K g h InfoState) -> K f h InfoState }

-- | 'Q' yields an instance of the /PMonad/ class of Orchard, Petricek, and
-- Mycroft.
instance PMonad (Q h) where
  return :: a -> Q h e e a
  return a = Q $ \k -> k a

  (>>=) :: Q h f e a -> (a -> Q h e g b) -> Q h f g b
  m >>= f = Q $ \k -> runQ m $ \x -> (runQ (f x)) k

-- | A makeshift parameterized bind to avoid naming conflicts outside
-- the current module.
(>>=>>) :: Q h f e a -> (a -> Q h e g b) -> Q h f g b
m >>=>> f = Q $ \k -> runQ m $ \x -> runQ (f x) k

-- | The parameterized monad operator 'upQ' is just @pure@.
upQ :: a -> Q h e e a
upQ a = Q $ \k -> k a

-- | The parameterized monad operator 'downQ' is just @(<*>)@.
downQ :: Q h f e (a -> b) -> Q h e g a -> Q h f g b
downQ u = \v -> joinQ $ fmap (\f -> fmap (\x -> f x) v) u

-- | The parameterized monad operator 'joinQ' is just @join@.
joinQ :: Q h f e (Q h e g b) -> Q h f g b
joinQ m = m >>=>> id

-- | For any three chosen parameters, 'Q' is an instance of the /Functor/ class.
instance Functor (Q h e f) where
  fmap f m = Q $ \k -> runQ m (k . f)

-- | Lift a binary operation on values into its Q-sensitive variant.
liftOpQ :: (a -> b -> c) -> Q h e f a -> Q h f g b -> Q h e g c
liftOpQ op = \m n -> joinQ $ fmap (\a -> fmap (\b -> op a b) n) m

-- | A function 'liftK' to transform objects in 'K' to objects in 'Q'.
liftK :: K e f a -> Q h e f a
liftK m = Q $ \k -> m >>=> k

-- | A function 'lowerQ' to transform information states in 'Q' to information
-- states in 'K'.
lowerQ :: Q h e h InfoState -> K e h InfoState
lowerQ m = runQ m upK

-- | A function 'resetQ' to zap the scopal and anaphoric potential of objects in
-- 'Q'.
resetQ :: Q f e f InfoState -> Q h e f InfoState
resetQ = liftK . lowerQ

-- | A function 'negS' contributing sentence-level negation and squashing the
-- scope and anaphoric potential of any indefinites in its scope.
-- negS :: Joinable P e f => K e '[] InfoState -> K (e :++ f) f InfoState
negS phi = monadicLift $ D $ GReaderT $ \i -> GReaderT $ \c ->
             runGReaderT (((runGReaderT . runD) $
               fmap neg (monadicLower phi)) i) c

-- | A function 'every' which provides the meaning of the corresponding English
-- determiner. (See Charlow 2014, Def. 3.4, p. 54, for the meaning of the
-- determiner /every/ upon which this one is based.)
every :: (Joinable P (e :++ '[]) '[],
          Joinable P e '[],
          Joinable P '[] (e :++ '[])) =>
         Lift (Entity -> Bool) -> Q '[] ((e :++ '[]) :++ '[]) e (Lift Entity)
every p = Q $ \k -> negS $ a p >>=> \x -> negS $ k x
