{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.CT.Interface where

import ForSyDe.Atom.MoC ((-.-), (-*-))
import ForSyDe.Atom.MoC.CT.Core as CT
import ForSyDe.Atom.MoC.DE.Core as DE
import ForSyDe.Atom.MoC.DE.Interface (toCT)
import ForSyDe.Atom.Utility (($$),($$$),($$$$))


------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import ForSyDe.Atom.MoC.DE.Lib as DE
-- >>> import ForSyDe.Atom.MoC.CT.Lib as CT
-- >>> import qualified Data.Number.FixedFunctions as RatF
-- >>> let pi'  = realToFrac pi
-- >>> let sin' = RatF.sin 0.001
-- >>> let cos' = RatF.cos 0.001


------- MoC INTERFACES -------


eventToDE :: CT.CT a -> DE.DE a
eventToDE e = DE.DE tg (CT.evalTs tg e)
  where tg = CT.tag e

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.CT.CT' signal(s) with a
-- 'ForSyDe.Atom.MoC.DE.DE' carrier which holds the timestamps at
-- which the CT signal must be evaluated, and outputs the respective
-- (set of) 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- The following constructors are provided:
--
-- > toDE, toDE2, toDE3, toDE4
--
-- >>> let s = CT.infinite (fromRational . sin')
-- >>> let c = DE.generate1 id (pi'/2, 1)
-- >>> takeS 6 $ toDE c s
-- { 0.0 @0s, 1.0 @1.570796326794s, 1.793238520564752e-12 @3.141592653588s, -1.0 @4.712388980382s, 0.0 @6.283185307176s, 1.0 @7.85398163397s}
--
-- <<docfiles/figs/moc-ct-tode.png>>
toDE2 :: DE.Signal t -- ^ 'ForSyDe.Atom.MoC.DE.DE' timestamp carrier 
      -> CT.Signal a -- ^ 'ForSyDe.Atom.MoC.CT.CT' input
      -> CT.Signal b -- ^ 'ForSyDe.Atom.MoC.CT.CT' input
      -> (DE.Signal a, DE.Signal b) -- ^ 'ForSyDe.Atom.MoC.DE.DE' outputs
toDE :: DE.Signal t
     -> CT.Signal a
     -> DE.Signal a
toDE3 :: DE.Signal t
      -> CT.Signal a -> CT.Signal b -> CT.Signal c
      -> (DE.Signal a, DE.Signal b, DE.Signal c)
toDE4 :: DE.Signal t
      -> CT.Signal a -> CT.Signal b -> CT.Signal c -> CT.Signal d
      -> (DE.Signal a, DE.Signal b, DE.Signal c, DE.Signal d)

toDE  carrier = fmap eventToDE . sync carrier
  where sync c s  = (\_ x -> x) -.- (toCT c) -*- s
toDE2 c s1 s2       = (toDE c s1, toDE c s2)
toDE3 c s1 s2 s3    = (toDE c s1, toDE c s2, toDE c s3) 
toDE4 c s1 s2 s3 s4 = (toDE c s1, toDE c s2, toDE c s3, toDE c s4)


                      
-- Towards skeleton layer

-- -- | Synchronizes all signals inside a vector into one signal carrying
-- -- vector events. This is simply an instantiation of the skeleton
-- -- 'V.zipx', which passes the CT context wrappers ('CT.wrap22') to
-- -- instantiate a 'ForSyDe.Atom.MoC.comb22' properly.
-- zipx :: V.Vector (CT.Signal a) -> CT.Signal (V.Vector a)
-- zipx = V.zipx CT.wrap21 CT.wrap11


-- -- | Unfolds a signal carrying vector events into a vector of synchronized
-- -- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- -- which passes the CT context wrappers ('CT.wrap22') to instantiate a
-- -- 'ForSyDe.Atom.MoC.comb22' properly.
-- --
-- -- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- -- it assumes all events are carrying vectors of the same length.
-- unzipx :: CT.Signal (V.Vector a) -> V.Vector (CT.Signal a)
-- unzipx = V.unzipx (CT.sniff) CT.wrap11 CT.wrap11

