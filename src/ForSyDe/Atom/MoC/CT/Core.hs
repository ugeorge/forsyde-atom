{-# LANGUAGE TypeFamilies, RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the CT MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT.Core where

import ForSyDe.Atom.MoC.Timed
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior

import Numeric
import Data.Ratio

type Time = Rational

-- | Type alias for a CT signal
type Sig   a = S.Signal (Event a)
type Event a = CT (Value a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data CT a = CT Time (Time -> a) 

-- | Implenents the CT semantics for the MoC atoms
instance MoC CT where
  ---------------------
  _ -$- NullS = NullS
  f -$- (x:-xs) = fmap f x :- f -$- xs
  ---------------------
  -- (-*-) :: Signal (CT Time (Time -> (Value a -> b))) -> Signal (CT Time (Time -> Value a)) -> Signal (CT Time (Time -> b))
  sf -*- sx = init (CT 0.0 (\_ -> Undef)) sf sx
    where init px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> f  <*> ue :- init    x  s1 xs
          comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (CT _ v) ->- xs = CT 0 v :- xs
  ---------------------
  (CT t _) -&- xs = (\(CT t1 v) -> CT (t1 + t) v) <$> xs

-- | Shows the event with tag @t@ and value @v@ as @(\@t:v)@
instance Show a => Show (CT a) where
  showsPrec _ (CT t x) = (++) ( show (x t) ++ "@" ++ (showFFloat Nothing $ fromRat t) "" )


-- | Needed to implement the @unzip@ utilities
instance Functor CT where
  fmap f (CT t a) = CT t (f <$> a)


instance Applicative CT where
  pure a = CT 0.0 (\_ -> a)
  (CT t f) <*> (CT _ x) = CT t (\a -> (f a) (x a))

-----------------------------------------------------------------------------

-- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event       :: (Rational, Rational -> a) -> Event a
event (t,f) = CT t (Value . f)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal   :: [(Rational, Rational -> a)] -> Sig a
signal l = S.signal $ event <$> l

partition :: Rational -> Sig a -> Sig a 
partition _    NullS     = NullS
partition sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t < nt    = CT t f :- chunk (CT (t+sample) f) s
          | otherwise = CT nt nf :- chunk (CT nt nf) fs
        chunk _ NullS = NullS

partitionUntil _ _ NullS = NullS
partitionUntil until sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t <= until && t < nt  = CT t f :- chunk (CT (t+sample) f) s
          | t <= until && t >= nt = CT nt nf :- chunk (CT nt nf) fs
          | otherwise = NullS
        chunk (CT t f) NullS
          | t <= until = CT t f :- chunk (CT (t+sample) f) NullS
          | otherwise  = NullS

tag (CT t _) = t 

infixl 7 %>
(%>) :: CT a -> CT b -> CT b
(CT t _) %> (CT _ x) = CT t x

ue = CT 0.0 (\_ -> Undef) :: Event x
----------------------------------------------------------------------------- 
