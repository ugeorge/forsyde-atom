{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Core where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility (($$),($$$),($$$$))

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Signal a   = Stream (DE a)

-- | The DE event. It identifies a discrete event signal.
data DE a  = DE { tag :: TimeStamp,  -- ^ timestamp
                  val :: a           -- ^ the value
                } deriving (Eq)

-- | Implenents the execution and synchronization semantics for the DE
-- MoC through its atoms.
instance MoC DE where
  type Fun DE a b = a -> b
  type Ret DE b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs)               =      f  <*> x  :- comb f  x  fs xs
    where comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f  px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x  NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (-*) = id
  ---------------------
  (DE _ v :- _) -<- xs = pure v :- xs
  ---------------------
  (_ :- DE d _ :- _) -&- xs = (\(DE t v) -> DE (t + d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.DE] signal delayed to infinity"
  ---------------------

-- | Shows the event with tag @t@ and value @v@ as @ v \@t@.
instance Show a => Show (DE a) where
  showsPrec _ (DE t x) = (++) ( " " ++ show x ++ " @" ++ show t )

-- | Reads the string of type @v\@t@ as an event @DE t v@.
instance (Read a) => Read (DE a) where
  readsPrec _ x = [ (DE tg val, r2)
                  | (val,r1) <- reads $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]

-- | Allows for mapping of functions on a DE event.
instance Functor DE where
  fmap f (DE t a) = DE t (f a)

-- | Allows for lifting functions on a pair of DE events.
instance Applicative DE where
  pure = DE 0
  (DE tf f) <*> (DE _ x) = DE tf (f x)

-----------------------------------------------------------------------------

unit  :: (TimeStamp, a) -> Signal a 
unit (t,v) = (DE 0 v :- DE t v :- NullS)

-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent
-- unit signal(s), in this case a signal with one event with the
-- period @tag@ carrying @value@.
--
-- The following helpers are exported:
--
-- > unit, unit2, unit3, unit4,
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: a -> Signal a
infinite v = DE 0 v :- NullS

-- | Transforms a list of tuples @(tag, value)@ into a DE
-- signal. Checks if it is well-formed.
signal :: [(TimeStamp, a)] -> Signal a
signal = checkSignal . stream . fmap (\(t, v) -> DE t v)

-- | Reads a signal from a string and checks if it is well-formed.
-- Like with the @read@ function from @Prelude@, you must specify the
-- type of the signal.
--
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- { 1 @0s, 2 @2s, 3 @5s, 4 @7s, 5 @10s}
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@10, 5@7 }" :: Signal Int
-- { 1 @0s, 2 @2s, 3 @5s*** Exception: [MoC.DE] malformed signal
-- >>> readSignal "{ 1@1, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- *** Exception: [MoC.DE] signal does not start from global 0
readSignal :: Read a => String -> Signal a
readSignal s = checkSignal $ read s

-- | Checks if a signal is well-formed or not, according to the DE MoC
-- interpretation in @ForSyDe-Atom@.
checkSignal NullS = NullS
checkSignal s@(x:-_)
  | tag x == 0 = checkOrder s
  | otherwise  = error "[MoC.DE] signal does not start from global 0"
  where
    checkOrder NullS      = NullS
    checkOrder (x:-NullS) = (x:-NullS)
    checkOrder (x:-y:-xs) | tag x < tag y = x :-checkOrder (y:-xs)
                          | otherwise = error "[MoC.DE] malformed signal"

----------------------------------------------------------------------------- 
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(DE t _) %> (DE _ x) = DE t x

-- end of testbench functions
-----------------------------------------------------------------------------
