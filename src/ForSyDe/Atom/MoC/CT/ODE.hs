-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT.ODE
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is a collection of numerical ordinary differential
-- equation (ODE) solvers.
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT.ODE (
  euler
  ) where

import ForSyDe.Atom.MoC.Time

-- | Numerical ODE solver using the Euler's method.
euler :: (Num a, Num b)
      -> TimeStamp   -- ^ time step/resolution
      -> (a -> b)    -- ^ the polinomial term _P_
      -> (Time -> a) -- ^ the function
      -> Rational    -- ^ the "history" of the integral at t0, i.e. start value
      -> TimeStamp   -- ^ t0, the start time of observing the system
      -> Time -> b
euler step poli f p t0 t = iterate p t0
  where
    bT = time step
    calc vp v = (bT*rc)/(bT+rc) * (1/rc * v + 1/bT * vp)
    iterate st ti
      | t < time ti = st
      | otherwise    = iterate (calc st $ f (time ti)) (ti + step)


-- | Trapezoidal method.
trap t0 y0 p1 t a = (b, p1'')
  where
    b          = y0 + (t - t0)/2 * (fa + fb)
    (fa, p1')  = prCT p1 t0 (a, y0)
    (fb, p1'') = prCT p1' t (a, y0)



intCT solver t0 y0 p1 = PCT {prCT = p}
  where
    p t a = (b, intCT solver t b p')
      where
        (b, p') = solver t0 y0 p1 t a
