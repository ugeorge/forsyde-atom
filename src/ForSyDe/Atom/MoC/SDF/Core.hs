{-# LANGUAGE TypeFamilies, RankNTypes, PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SDF.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.SDF.Core where

import           ForSyDe.Atom.Behavior
import           ForSyDe.Atom.MoC.Untimed
import           ForSyDe.Atom.Signal as S

import qualified Data.Param.FSVec as V
import           Data.TypeLevel hiding ((==))


type Sig a    = S.Signal (Token a)
type Token a  = SDF (Value a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data SDF a = SDF a

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  _ -$- NullS = NullS
  f -$- xs    = let x'  = extract c xs 
                    xs' = dropS (toInt c) xs
                    c   = V.lengthT x'
                in if toInt c == length (V.fromVector x')
                   then SDF (f $ pack x') :- f -$- xs'
                   else NullS
  ---------------------
  _ -*- NullS = NullS
  NullS -*- _ = NullS
  (SDF f :- fs) -*- xs = let x'  = extract c xs
                             xs' = dropS (toInt c) xs
                             c   = V.lengthT x'
                         in if toInt c == length (V.fromVector x')
                            then SDF (f $ pack x') :- fs -*- xs'
                            else NullS
  ---------------------
  post = S.signal . fmap SDF . concat . fmap unpack . (fmap . fmap) V.fromVector . fmap fromSDF . S.fromSignal
  ---------------------
  (-&>-) =  (+-+) . S.signal . fmap SDF . unpack . fmap V.fromVector . fromSDF
  ---------------------


-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor SDF where
  fmap f (SDF a) = SDF (f a)
 
-- | Shows the (extended) value wrapped
instance Show a => Show (SDF a) where
  showsPrec _ (SDF x) = (++) (show x)

-----------------------------------------------------------------------------

-- | Wraps a base value into a SY event of extended values
event    :: a -> Token a
event    = SDF . Value 

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal (event <$> l)

-----------------------------------------------------------------------------

fromSDF (SDF a) = a

pack ::(Nat s) => V.FSVec s (Value a) -> Value (V.FSVec s a)
pack = fmap V.FSVec . transpose . V.fromVector
  where transpose []     = Value []
        transpose (a:as) = (:) <$> a <*> transpose as

unpack :: Value [a] -> [Value a]
unpack Abst       = []
unpack Undef      = []
unpack (Value as) = Value <$> as


extract :: (Nat s) => s -> Signal (SDF (Value a)) -> V.FSVec s (Value a)
extract c = V.reallyUnsafeVector . take (toInt c) . fmap fromSDF . fromSignal

-----------------------------------------------------------------------------
