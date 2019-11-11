-- | This is what the world is like.
{-# LANGUAGE
    FlexibleInstances,
    FlexibleContexts,
    TypeOperators,
    MultiParamTypeClasses,
    KindSignatures,
    UndecidableInstances,
    UndecidableSuperClasses,
    DataKinds #-}

module Model where

import Prelude hiding (Monad(..))
import Control.Effect
import Data.Type.Set ((:++))

data Entity = E1  | E2  | E3  | E4  | E5  | E6
            | E7  | E8  | E9  | E10 | E11 | E12
            | E13 | E14 | E15 | E16 | E17 | E18
            | E19 | E20 | E21 | E22 | E23 | E24
            | E25 | E26 | E27 | E28 | E29 | E30
            deriving (Show, Eq, Bounded, Enum)

entities :: [Entity]
entities = [minBound..maxBound]

type SeaAnimal = Entity -> Bool
type CafeItem = Entity -> Bool

crab, seahorse, seacucumber, eel, clam, haddock, octopus, squid, stingray :: SeaAnimal

crab = \x -> elem x [E1 ..E3]
seahorse = \x -> elem x [E4 ..E6]
seacucumber = \x -> elem x [E7]
eel = \x -> elem x [E8]
clam = \x -> elem x [E9, E10]
haddock = \x -> elem x [E11 ..E13]
octopus = \x -> elem x [E14, E15]
squid = \x -> elem x [E16]
stingray = \x -> elem x [E17]

mollusk = \x -> clam x || octopus x || squid x
crustacean = crab
fish = \x -> seahorse x || eel x || haddock x
animal = \x -> mollusk x || crustacean x || fish x || stingray x

ashley, matt, tj, tony, curtis, sally :: Entity

ashley = E2
matt = E5
tj = E16
tony = E11
curtis = E7
sally = E14

latte, cappuccino, espresso, cortado, drip, tiramisu, biscotti, pannacotta, cannoli :: CafeItem

latte = \x -> elem x [E18, E19]
cappuccino = \x -> elem x [E20]
espresso = \x -> elem x [E21]
cortado = \x -> elem x [E22]
drip = \x -> elem x [E23, E24]
tiramisu = \x -> elem x [E25, E26]
biscotti = \x -> elem x [E27]
pannacotta = \x -> elem x [E28, E29]
cannoli = \x -> elem x [E30]

coffee = \x -> latte x || cappuccino x || espresso x || cortado x || drip x
espressodrink = \x -> latte x || cappuccino x || espresso x || cortado x
drink = coffee
dessert = \x -> tiramisu x || biscotti x || pannacotta x || cannoli x
edible = dessert
cafeitem = \x -> drink x || edible x

electric, giant, venomous, lazy, thirsty :: SeaAnimal -> SeaAnimal

electric = \p x -> p x && (eel x || stingray x)
giant = \p x -> p x && (x == E4 || x == E12 || octopus x)
venomous = \p x -> p x && (x == E8 || stingray x)
lazy = \p x -> p x && (x == E12 || x == E13 || seacucumber x)
thirsty = \p x -> p x && (x == E16 || x == E2 || x == E14)
scaled = \p x -> p x && haddock x

hot, iced, creamy, skim, sweet, stale :: CafeItem -> CafeItem

hot = \p x -> p x && (x == E18 || x == E20 || x == E23 || x == E30 || cortado x || espresso x)
iced = \p x -> p x && (x == E19 || x == E24)
creamy = \p x -> p x && (tiramisu x || pannacotta x || cannoli x)
skim = \p x -> p x && x == E19
sweet = \p x -> p x && (dessert x || x == E18)
stale = \p x -> p x && (x == E27 || drip x)

sipped, ate, spilled, attacked, stung, gobbled :: Entity -> Entity -> Bool

sipped = \x y -> iced latte x && y == E3
ate = \x y -> cannoli x && y == E14
spilled = \x y -> (espressodrink x && y == E16) || (x == E23 && y == E11)
attacked = \x y -> (x == E17 && y == E7) || (x == E9 && seahorse y)
stung = \x y -> animal x && y == E17
gobbled = \x y -> (dessert x && clam y) || (elem x [E25, E29] && y == E8)

gave, bought, saved, recommended:: Entity -> Entity -> Entity -> Bool

gave = \x y z -> x == E16 && y == E22 && z == E1
bought = \x y z -> x == E16 && espressodrink y && z == E17
saved = \x y z -> x == E7 && hot cannoli y && z == E7
recommended = \x y z -> fish x && creamy coffee y && mollusk z

was :: ((Entity -> Bool) -> Entity -> Bool) -> Entity -> Bool
was adj = adj $ \x -> True

-- | Forward application.
fa :: (a -> b) -> a -> b
fa = ($)

-- | Backward application.
ba :: a -> (a -> b) -> b
ba = flip ($)
