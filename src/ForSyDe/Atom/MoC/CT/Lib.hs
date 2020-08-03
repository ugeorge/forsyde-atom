{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.CT.Lib
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a set of helpers for properly instantiating
-- process network patterns as process constructors.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.CT.Lib where

import qualified ForSyDe.Atom.MoC as MoC
import           ForSyDe.Atom.MoC.CT.Core
import           ForSyDe.Atom.Utility.Plot
import           ForSyDe.Atom.Utility.Tuple

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import ForSyDe.Atom.MoC.tm as tm
-- >>> import ForSyDe.Atom.MoC.ts as ts
-- >>> let pi'  = ts.pi
-- >>> let exp' = tm.exp
-- >>> let sin' = tm.sin
-- >>> let cos' = tm.cos
-- >>> let cfg  = defaultCfg {xmax=10, rate=0.1}

------- DELAY -------

-- | The @delay@ process "delays" a signal with one
-- event. Instantiates the 'ForSyDe.Atom.MoC.delay' pattern. In the CT
-- MoC, this process can be interpreted as an ideal "delay line".
--
-- >>> let s  = infinite (sin')
-- >>> let s' = delay 2 (\_ -> 0) s
-- >>> dumpDat $ prepare cfg {labels=["delay"]} $ s'
-- Dumped delay in ./fig
-- ["./fig/delay.dat"]
--
-- <<fig/moc-ct-pattern-delay.png>>
delay :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => ts   -- ^ time delay
      -> (tm -> a) -- ^ initial value
      -> SignalBase ts tm a    -- ^ input signal
      -> SignalBase ts tm a    -- ^ output signal

delay t v = MoC.delay (unit (t, v))

-- | Similar to the previous, but this is the raw instantiation of the
-- 'ForSyDe.Atom.MoC.delay' pattern. It "borrows" the first event from
-- one signal and appends it at the head of another signal.
--
-- >>> let s  = infinite (sin')
-- >>> let s' = signal [(0, \_ -> 0), (2, \_ -> 1)]
-- >>> dumpDat $ prepare cfg {labels=["delayp"]} $ delay' s' s
-- Dumped delayp in ./fig
-- ["./fig/delayp.dat"]
--
-- <<fig/moc-ct-pattern-delayp.png>>
delay' :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm a  -- ^ signal "borrowing" the initial event
      -> SignalBase ts tm a   -- ^ input signal
      -> SignalBase ts tm a   -- ^ output signal

delay' = MoC.delay

------- COMB -------

-- | @comb@ processes map combinatorial functions on signals and take
-- care of synchronization between input signals. It instantiates the
-- @comb@ pattern (see 'ForSyDe.Atom.MoC.comb22').
-- 
-- Constructors: @comb[1-4][1-4]@.
--
-- >>> let s1 = infinite (sin')
-- >>> let s2 = signal [(0,\_->0),(pi',\_->1),(2*pi',\_->0),(3*pi',\_->1)]
-- >>> let o1 = comb11 (+1) s2
-- >>> let (o2_1, o2_2) = comb22 (\a b-> (a+b,a*b)) s1 s2
-- >>> dumpDat $ prepare cfg {labels=["comb11"]} o1  
-- Dumped comb11 in ./fig
-- ["./fig/comb11.dat"]
-- >>> dumpDat $ prepareL cfg {labels=["comb22-1","comb22-2"]} [o2_1, o2_2]
-- Dumped comb22-1, comb22-2 in ./fig
-- ["./fig/comb22-1.dat","./fig/comb22-2.dat"]
--
-- <<fig/moc-ct-pattern-comb.png>>
comb22 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> (b1, b2)) -- ^ function on values
       -> SignalBase ts tm a1              -- ^ first input signal
       -> SignalBase ts tm a2              -- ^ second input signal
       -> (SignalBase ts tm b1, SignalBase ts tm b2) -- ^ two output signals
comb11 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> b1)
       -> SignalBase ts tm a1 -> SignalBase ts tm b1                                
comb12 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> (b1, b2))
       -> SignalBase ts tm a1 -> (SignalBase ts tm b1, SignalBase ts tm b2)                          
comb13 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> (b1, b2, b3))
       -> SignalBase ts tm a1 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                      
comb14 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> (b1, b2, b3, b4))
       -> SignalBase ts tm a1 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                  
comb21 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> b1)
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm b1                          
comb23 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> (b1, b2, b3))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                
comb24 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> (b1, b2, b3, b4))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)            
comb31 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> b1)
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm b1                    
comb32 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> (b1, b2))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> (SignalBase ts tm b1, SignalBase ts tm b2)              
comb33 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> (b1, b2, b3))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)          
comb34 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> (b1, b2, b3, b4))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)     
comb41 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> a4 -> b1)
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 -> SignalBase ts tm b1              
comb42 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> a4 -> (b1, b2))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 -> (SignalBase ts tm b1, SignalBase ts tm b2)        
comb43 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)    
comb44 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
       -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

comb11 = MoC.comb11 
comb12 = MoC.comb12 
comb13 = MoC.comb13 
comb14 = MoC.comb14 
comb21 = MoC.comb21 
comb22 = MoC.comb22 
comb23 = MoC.comb23 
comb24 = MoC.comb24 
comb31 = MoC.comb31 
comb32 = MoC.comb32 
comb33 = MoC.comb33 
comb34 = MoC.comb34 
comb41 = MoC.comb41 
comb42 = MoC.comb42 
comb43 = MoC.comb43 
comb44 = MoC.comb44 

------- RECONFIG -------

-- | @reconfig@ creates a CT adaptive process where the first signal
-- carries functions and the other carry the arguments. It
-- instantiates the @reconfig@ atom pattern (see
-- 'ForSyDe.Atom.MoC.reconfig22').
--
-- Constructors: @reconfig[1-4][1-4]@.
--
-- >>> let s1 = infinite (sin')
-- >>> let sf = signal [(0,\_->(*0)),(pi',\_->(+1)),(2*pi',\_->(*0)),(3*pi',\_->(+1))]
-- >>> dumpDat $ prepare cfg {labels=["reconfig"]} $ reconfig11 sf s1
-- Dumped reconfig in ./fig
-- ["./fig/reconfig.dat"]
reconfig22 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm (a1 -> a2 -> (b1, b2))
           -- ^ signal carrying functions
           -> SignalBase ts tm a1
           -- ^ first input signal carrying arguments
           -> SignalBase ts tm a2
           -- ^ second input signal carrying arguments
           -> (SignalBase ts tm b1, SignalBase ts tm b2)
           -- ^ two output signals
reconfig11 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm (a1 -> b1)
           -> SignalBase ts tm a1
           -> SignalBase ts tm b1
reconfig12 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> (b1, b2))
           -> SignalBase ts tm a1
           -> (SignalBase ts tm b1, SignalBase ts tm b2)
reconfig13 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> (b1, b2, b3))
           -> SignalBase ts tm a1
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)
reconfig14 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> (b1, b2, b3, b4))
           -> SignalBase ts tm a1
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)
reconfig21 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> b1)
           -> SignalBase ts tm a1 -> SignalBase ts tm a2
           -> SignalBase ts tm b1
reconfig23 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> (b1, b2, b3))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)
reconfig24 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> (b1, b2, b3, b4))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)
reconfig31 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> b1)
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
           -> SignalBase ts tm b1
reconfig32 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> (b1, b2))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
           -> (SignalBase ts tm b1, SignalBase ts tm b2)
reconfig33 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> (b1, b2, b3))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)
reconfig34 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> (b1, b2, b3, b4))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)
reconfig41 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> a4 -> b1)
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
           -> SignalBase ts tm b1
reconfig42 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> a4 -> (b1, b2))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
           -> (SignalBase ts tm b1, SignalBase ts tm b2)
reconfig43 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)
reconfig44 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => SignalBase ts tm(a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
           -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

reconfig11 = MoC.reconfig11 
reconfig12 = MoC.reconfig12 
reconfig13 = MoC.reconfig13 
reconfig14 = MoC.reconfig14 
reconfig21 = MoC.reconfig21 
reconfig22 = MoC.reconfig22 
reconfig23 = MoC.reconfig23 
reconfig24 = MoC.reconfig24 
reconfig31 = MoC.reconfig31 
reconfig32 = MoC.reconfig32 
reconfig33 = MoC.reconfig33 
reconfig34 = MoC.reconfig34 
reconfig41 = MoC.reconfig41 
reconfig42 = MoC.reconfig42 
reconfig43 = MoC.reconfig43 
reconfig44 = MoC.reconfig44 

------- CONSTANT -------

-- | A generator for a constant signal. As compared with the
-- 'ForSyDe.Atom.MoC.SY.constant2', it just constructs an infinite
-- signal with constant value (i.e. a signal with one event starting
-- from time 0).
--
-- Constructors: @constant[1-4]@.
--
-- >>> dumpDat $ prepare cfg {labels=["constant"]} $ constant1 2
-- Dumped constant in ./fig
-- ["./fig/constant.dat"]
--
-- <<fig/moc-ct-pattern-constant.png>>
constant2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (b1, b2)         -- ^ values to be repeated
          -> (SignalBase ts tm b1, SignalBase ts tm b2) -- ^ generated signals
constant1 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => b1 -> SignalBase ts tm b1                                
constant3 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (b1, b2, b3)
          -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)
constant4 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (b1, b2, b3, b4)
          -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

constant1 = infinite . const 
constant2 = ($$) (constant1, constant1)
constant3 = ($$$) (constant1,constant1,constant1)
constant4 = ($$$$) (constant1,constant1,constant1,constant1)


------- INFINITE -------

-- | A generator for an infinite signal. Similar to 'constant2'.
--
-- Constructors: @infinite[1-4]@.
--
-- >>> let (inf1, inf2) = infinite2 (sin', cos')
-- >>> dumpDat $ prepareL cfg {labels=["infinite2-1", "infinite2-2"]} [inf1, inf2] 
-- Dumped infinite2-1, infinite2-2 in ./fig
-- ["./fig/infinite2-1.dat","./fig/infinite2-2.dat"]
--
-- <<fig/moc-ct-pattern-infinite.png>>
infinite2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (tm -> b1, tm -> b2)         -- ^ values to be repeated
          -> (SignalBase ts tm b1, SignalBase ts tm b2) -- ^ generated signals
infinite1 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (tm -> b1) -> SignalBase ts tm b1                                
infinite3 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (tm -> b1, tm -> b2, tm -> b3)
          -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)
infinite4 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
          => (tm -> b1, tm -> b2, tm -> b3, tm -> b4)
          -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

infinite1 = infinite
infinite2 = ($$) (infinite, infinite)
infinite3 = ($$$) (infinite,infinite,infinite)
infinite4 = ($$$$) (infinite,infinite,infinite,infinite)

------- GENERATE -------

-- | A signal generator based on a function and a kernel value. It
-- is actually an instantiation of the @stated0X@ constructor
-- (check 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @generate[1-4]@.
--
-- >>> let { osc 0 = 1 ; osc 1 = 0 }
-- >>> dumpDat $ prepare cfg {labels=["generate"]} $ generate1 osc (pi', \_->0)
-- Dumped generate in ./fig
-- ["./fig/generate.dat"]
--
-- <<fig/moc-ct-pattern-generate.png>>
--
-- Another example simulating an RC oscillator:
--
-- >>> let vs = 2                                -- Vs : supply voltage
-- >>> let r  = 100                              -- R : resistance
-- >>> let c  = 0.0005                           -- C : capacitance
-- >>> let vc t = vs * (1 - exp' (-t / (r * c))) -- Vc(t) : voltage charging through capacitor
-- >>> let ns v = vs + (-1 * v)                  -- next state : charging / discharging
-- >>> let rcOsc = generate1 ns (milisec 500, vc)
-- >>> dumpDat $ prepare cfg {labels=["rcosc"]} $ rcOsc
-- Dumped rcosc in ./fig
-- ["./fig/rcosc.dat"]
--
-- <<fig/moc-ct-pattern-generate1.png>>
generate2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> (b1, b2))
          -- ^ function to generate next value
          -> ((ts, tm -> b1), (ts, tm -> b2))
          -- ^ kernel values tupled with their generation rate.
          -> (SignalBase ts tm b1, SignalBase ts tm b2) -- ^ generated signals
generate1 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b1) -> (ts, tm -> b1)
          -> SignalBase ts tm b1                                
generate3 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> (b1, b2, b3))
          -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
          -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                      
generate4 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> b4 -> (b1, b2, b3, b4))
          -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
          -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                  

generate1 ns i = MoC.stated01 ns (unit  i)
generate2 ns i = MoC.stated02 ns (unit2 i)
generate3 ns i = MoC.stated03 ns (unit3 i)
generate4 ns i = MoC.stated04 ns (unit4 i)

------- STATED -------

-- | @stated@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @stated[1-4][1-4]@.
--
-- >>> let { osc 0 a = a; osc _ a = 0 }   
-- >>> let s1 = signal [(0,\_->1), (6,\_->0)]
-- >>> dumpDat $ prepare cfg {labels=["stated"]} $ stated11 osc (1,\_->0) s1
-- Dumped stated in ./fig
-- ["./fig/stated.dat"]
--
-- <<fig/moc-ct-pattern-stated.png>>
stated22 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> a1 -> a2 -> (b1, b2))
            -- ^ next state function
           -> ((ts, tm -> b1), (ts, tm -> b2))
           -- ^ initial state values tupled with their initial delay
            -> SignalBase ts tm a1
            -- ^ first input signal
            -> SignalBase ts tm a2
            -- ^ second input signal
            -> (SignalBase ts tm b1, SignalBase ts tm b2) -- ^ output signals
stated11 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> a1 -> b1)
         -> (ts, tm -> b1)
         -> SignalBase ts tm a1
         -> SignalBase ts tm b1 
stated12 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> a1 -> (b1, b2))
         -> ((ts, tm -> b1), (ts, tm -> b2))
         -> SignalBase ts tm a1
         -> (SignalBase ts tm b1, SignalBase ts tm b2) 
stated13 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
         -> SignalBase ts tm a1
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3) 
stated14 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
         -> SignalBase ts tm a1
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4) 
stated21 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> a1 -> a2 -> b1)
         -> (ts, tm -> b1)
         -> SignalBase ts tm a1 -> SignalBase ts tm a2
         -> SignalBase ts tm b1 
stated23 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3) 
stated24 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4) 
stated31 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> a1 -> a2 -> a3 -> b1)
         -> (ts, tm -> b1)
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
         -> SignalBase ts tm b1 
stated32 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2))
         -> ((ts, tm -> b1), (ts, tm -> b2))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
         -> (SignalBase ts tm b1, SignalBase ts tm b2) 
stated33 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3) 
stated34 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4) 
stated41 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> a1 -> a2 -> a3 -> a4 -> b1)
         -> (ts, tm -> b1)
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
         -> SignalBase ts tm b1 
stated42 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2))
         -> ((ts, tm -> b1), (ts, tm -> b2))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
         -> (SignalBase ts tm b1, SignalBase ts tm b2) 
stated43 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3) 
stated44 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
         -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
         -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
         -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

stated11 ns i = MoC.stated11 ns (unit  i)
stated12 ns i = MoC.stated12 ns (unit2 i)
stated13 ns i = MoC.stated13 ns (unit3 i)
stated14 ns i = MoC.stated14 ns (unit4 i)
stated21 ns i = MoC.stated21 ns (unit  i)
stated22 ns i = MoC.stated22 ns (unit2 i)
stated23 ns i = MoC.stated23 ns (unit3 i)
stated24 ns i = MoC.stated24 ns (unit4 i)
stated31 ns i = MoC.stated31 ns (unit  i)
stated32 ns i = MoC.stated32 ns (unit2 i)
stated33 ns i = MoC.stated33 ns (unit3 i)
stated34 ns i = MoC.stated34 ns (unit4 i)
stated41 ns i = MoC.stated41 ns (unit  i)
stated42 ns i = MoC.stated42 ns (unit2 i)
stated43 ns i = MoC.stated43 ns (unit3 i)
stated44 ns i = MoC.stated44 ns (unit4 i)

------- STATE -------

-- | @state@ is a state machine without an output decoder, and the
-- state non-transparent. It is an instantiation of the @state@ MoC
-- constructor (see 'ForSyDe.Atom.MoC.state22').
--
-- Constructors: @state[1-4][1-4]@.
--
-- >>> let { osc 0 a = a; osc _ a = 0 }   
-- >>> let s1 = signal [(0,\_->1), (6,\_->0)]
-- >>> dumpDat $ prepare cfg {labels=["state"]} $ state11 osc (1,\_->0) s1
-- Dumped state in ./fig
-- ["./fig/state.dat"]
--
-- <<fig/moc-ct-pattern-state.png>>                   
state22 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> a1 -> a2 -> (b1, b2))
           -- ^ next state function
           -> ((ts, tm -> b1), (ts, tm -> b2))
           -- ^ initial state values tupled with their initial delay
           -> SignalBase ts tm a1
           -- ^ first input signal
           -> SignalBase ts tm a2
           -- ^ second input signal
           -> (SignalBase ts tm b1, SignalBase ts tm b2) -- ^ output signals
state11 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> a1 -> b1)
        -> (ts, tm -> b1)
        -> SignalBase ts tm a1
        -> SignalBase ts tm b1                                
state12 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> a1 -> (b1, b2)) 
        -> ((ts, tm -> b1), (ts, tm -> b2))
        -> SignalBase ts tm a1
        -> (SignalBase ts tm b1, SignalBase ts tm b2)                          
state13 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
        -> SignalBase ts tm a1
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                      
state14 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
        -> SignalBase ts tm a1 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                  
state21 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> a1 -> a2 -> b1)
        -> (ts, tm -> b1)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2
        -> SignalBase ts tm b1                          
state23 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                
state24 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
    => (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                     
state31 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> a1 -> a2 -> a3 -> b1)
        -> (ts, tm -> b1)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
        -> SignalBase ts tm b1                    
state32 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) 
        -> ((ts, tm -> b1), (ts, tm -> b2))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 
        -> (SignalBase ts tm b1, SignalBase ts tm b2)              
state33 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)          
state34 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)     
state41 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> a1 -> a2 -> a3 -> a4 -> b1)
        -> (ts, tm -> b1)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
        -> SignalBase ts tm b1
state42 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) 
        -> ((ts, tm -> b1), (ts, tm -> b2))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 
        -> (SignalBase ts tm b1, SignalBase ts tm b2)        
state43 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)    
state44 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) 
        -> ((ts, tm -> b1), (ts, tm -> b2), (ts, tm -> b3), (ts, tm -> b4))
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

state11 ns i = MoC.state11 ns (unit  i)
state12 ns i = MoC.state12 ns (unit2 i)
state13 ns i = MoC.state13 ns (unit3 i)
state14 ns i = MoC.state14 ns (unit4 i)
state21 ns i = MoC.state21 ns (unit  i)
state22 ns i = MoC.state22 ns (unit2 i)
state23 ns i = MoC.state23 ns (unit3 i)
state24 ns i = MoC.state24 ns (unit4 i)
state31 ns i = MoC.state31 ns (unit  i)
state32 ns i = MoC.state32 ns (unit2 i)
state33 ns i = MoC.state33 ns (unit3 i)
state34 ns i = MoC.state34 ns (unit4 i)
state41 ns i = MoC.state41 ns (unit  i)
state42 ns i = MoC.state42 ns (unit2 i)
state43 ns i = MoC.state43 ns (unit3 i)
state44 ns i = MoC.state44 ns (unit4 i)

------- MOORE -------

-- | @moore@ processes model Moore state machines. It is an
-- instantiation of the @moore@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.moore22').
--
-- Constructors: @moore[1-4][1-4]@.
--
-- >>> let { osc 0 a = a; osc _ a = 0 }   
-- >>> let s1 = signal [(0,\_->1), (6,\_->0)]
-- >>> dumpDat $ prepare cfg {labels=["moore"]} $ moore11 osc (*3) (1,\_->0) s1
-- Dumped moore in ./fig
-- ["./fig/moore.dat"]
--
-- <<fig/moc-ct-pattern-moore.png>>          
moore22 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st)
           -- ^ next state function
           -> (st -> (b1, b2))
           -- ^ output decoder
           -> (ts, tm -> st)
           -- ^ initial state: tag and value
           -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> (SignalBase ts tm b1, SignalBase ts tm b2)
moore11 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st)
        -> (st -> b1)
        -> (ts, tm -> st)
        -> SignalBase ts tm a1
        -> SignalBase ts tm b1                                
moore12 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st)
        -> (st -> (b1, b2))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1
        -> (SignalBase ts tm b1, SignalBase ts tm b2)                          
moore13 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st)
        -> (st -> (b1, b2, b3))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                      
moore14 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                  
moore21 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st)
        -> (st -> b1)
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2
        -> SignalBase ts tm b1                          
moore23 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st)
        -> (st -> (b1, b2, b3))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                
moore24 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                     
moore31 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st)
        -> (st -> b1)
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
        -> SignalBase ts tm b1                    
moore32 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st)
        -> (st -> (b1, b2))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
        -> (SignalBase ts tm b1, SignalBase ts tm b2)              
moore33 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st)
        -> (st -> (b1, b2, b3))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)          
moore34 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)     
moore41 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> b1)
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
        -> SignalBase ts tm b1              
moore42 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> (b1, b2))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
        -> (SignalBase ts tm b1, SignalBase ts tm b2)        
moore43 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> (b1, b2, b3))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)    
moore44 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

moore11 ns od i = MoC.moore11 ns od (unit i)
moore12 ns od i = MoC.moore12 ns od (unit i)
moore13 ns od i = MoC.moore13 ns od (unit i)
moore14 ns od i = MoC.moore14 ns od (unit i)
moore21 ns od i = MoC.moore21 ns od (unit i)
moore22 ns od i = MoC.moore22 ns od (unit i)
moore23 ns od i = MoC.moore23 ns od (unit i)
moore24 ns od i = MoC.moore24 ns od (unit i)
moore31 ns od i = MoC.moore31 ns od (unit i)
moore32 ns od i = MoC.moore32 ns od (unit i)
moore33 ns od i = MoC.moore33 ns od (unit i)
moore34 ns od i = MoC.moore34 ns od (unit i)
moore41 ns od i = MoC.moore41 ns od (unit i)
moore42 ns od i = MoC.moore42 ns od (unit i)
moore43 ns od i = MoC.moore43 ns od (unit i)
moore44 ns od i = MoC.moore44 ns od (unit i)

------- MEALY -------

-- | @mealy@ processes model Mealy state machines. It is an
-- instantiation of the @mealy@ MoC constructor
-- (see 'ForSyDe.Atom.MoC.mealy22').
--
-- Constructors: @mealy[1-4][1-4]@.
--
-- >>> let { osc (-1) _ = 1; osc 1 _ = (-1) } 
-- >>> let s1 = infinite sin'
-- >>> dumpDat $ prepare cfg {labels=["mealy"]} $ mealy11 osc (*) (pi',\_->1) s1
-- Dumped mealy in ./fig
-- ["./fig/mealy.dat"]
--
-- <<fig/moc-ct-pattern-mealy.png>>
mealy22 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st)
        -- ^ next state function
        -> (st -> a1 -> a2 -> (b1, b2))
        -- ^ outpt decoder
        -> (ts, tm -> st)
        -- ^ initial state: tag and value
        -> SignalBase ts tm a1 -> SignalBase ts tm a2
        -> (SignalBase ts tm b1, SignalBase ts tm b2)
mealy11 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st) 
        -> (st -> a1 -> b1) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1
        -> SignalBase ts tm b1                                
mealy12 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st) 
        -> (st -> a1 -> (b1, b2)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 
        -> (SignalBase ts tm b1, SignalBase ts tm b2)                          
mealy13 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st) 
        -> (st -> a1 -> (b1, b2, b3)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                      
mealy14 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> st) 
        -> (st -> a1 -> (b1, b2, b3, b4)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                  
mealy21 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st) 
        -> (st -> a1 -> a2 -> b1) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2
        -> SignalBase ts tm b1                          
mealy23 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st) 
        -> (st -> a1 -> a2 -> (b1, b2, b3)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)                
mealy24 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> st) 
        -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)                     
mealy31 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> b1) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
        -> SignalBase ts tm b1  
mealy32 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> (b1, b2)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 
        -> (SignalBase ts tm b1, SignalBase ts tm b2)              
mealy33 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)          
mealy34 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)     
mealy41 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> b1) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
        -> SignalBase ts tm b1
mealy42 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 
        -> (SignalBase ts tm b1, SignalBase ts tm b2)        
mealy43 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3)    
mealy44 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) 
        -> (ts, tm -> st)
        -> SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4 
        -> (SignalBase ts tm b1, SignalBase ts tm b2, SignalBase ts tm b3, SignalBase ts tm b4)

mealy11 ns od i = MoC.mealy11 ns od (unit i)
mealy12 ns od i = MoC.mealy12 ns od (unit i)
mealy13 ns od i = MoC.mealy13 ns od (unit i)
mealy14 ns od i = MoC.mealy14 ns od (unit i)
mealy21 ns od i = MoC.mealy21 ns od (unit i)
mealy22 ns od i = MoC.mealy22 ns od (unit i)
mealy23 ns od i = MoC.mealy23 ns od (unit i)
mealy24 ns od i = MoC.mealy24 ns od (unit i)
mealy31 ns od i = MoC.mealy31 ns od (unit i)
mealy32 ns od i = MoC.mealy32 ns od (unit i)
mealy33 ns od i = MoC.mealy33 ns od (unit i)
mealy34 ns od i = MoC.mealy34 ns od (unit i)
mealy41 ns od i = MoC.mealy41 ns od (unit i)
mealy42 ns od i = MoC.mealy42 ns od (unit i)
mealy43 ns od i = MoC.mealy43 ns od (unit i)
mealy44 ns od i = MoC.mealy44 ns od (unit i)

------- SYNC -------

-- | @sync@ synchronizes multiple signals, so that they have the same
-- set of tags, and consequently, the same number of events. It
-- instantiates the @comb@ atom pattern (see
-- 'ForSyDe.Atom.MoC.comb22').
--
-- Constructors: @sync[1-4]@.
sync2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => SignalBase ts tm a1                    -- ^ first input signal
      -> SignalBase ts tm a2                    -- ^ second input signal
      -> (SignalBase ts tm a1, SignalBase ts tm a2)       -- ^ two output signals
sync3 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3
      -> (SignalBase ts tm a1, SignalBase ts tm a2, SignalBase ts tm a3)             
sync4 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => SignalBase ts tm a1 -> SignalBase ts tm a2 -> SignalBase ts tm a3 -> SignalBase ts tm a4
      -> (SignalBase ts tm a1, SignalBase ts tm a2, SignalBase ts tm a3, SignalBase ts tm a4)

sync2 = comb22 (,)
sync3 = comb33 (,,)
sync4 = comb44 (,,,)
