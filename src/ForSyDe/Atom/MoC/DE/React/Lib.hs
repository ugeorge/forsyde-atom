{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.DE.React.Lib
-- Copyright   :  (c) George Ungureanu, KTH/EECS/ESY 2020
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
module ForSyDe.Atom.MoC.DE.React.Lib where

import qualified ForSyDe.Atom.ExB               as AE
import qualified ForSyDe.Atom.ExB.Absent        as AE
import ForSyDe.Atom.MoC (
  MoC(..), (-&>-), (-*<), (-*<<), (-*<<<),
  ctxt11, ctxt21, ctxt31, ctxt41, ctxt51, ctxt61, ctxt71, ctxt81, 
  ctxt12, ctxt22, ctxt32, ctxt42, ctxt52, ctxt62, ctxt72, ctxt82, 
  ctxt13, ctxt23, ctxt33, ctxt43, ctxt53, ctxt63, ctxt73, ctxt83, 
  ctxt14, ctxt24, ctxt34, ctxt44, ctxt54, ctxt64, ctxt74, ctxt84,
  ) 
import qualified ForSyDe.Atom.MoC               as MoC
import qualified ForSyDe.Atom.MoC.DE            as DE
import           ForSyDe.Atom.MoC.DE.React.Core as RE
import qualified ForSyDe.Atom.MoC.SY            as SY
import qualified ForSyDe.Atom.MoC.SY.Clocked    as SYC
import           ForSyDe.Atom.MoC.Stream (Stream(..))
import           ForSyDe.Atom.Utility.Tuple

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.SY.Lib as SY
-- >>> import ForSyDe.Atom.MoC.DE.Core (readSignal, signal)
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import ForSyDe.Atom.Utility.Plot

------- DELAY -------

-- | The @delay@ process "delays" a signal with one event, (see the
-- 'ForSyDe.Atom.MoC.delay' pattern). Any delayed signal starts from global 0.
--
-- >>> let s = readSignal "{1@2, 2@3, 3@6, 4@8, 5@9}" :: Signal Int
-- >>> delay 3 0 s
-- {0@0s,1@5s,2@6s,3@9s,4@11s,5@12s}
-- 
-- <<fig/moc-re-pattern-delay.png>>
delay :: (Num t, Ord t)
      => t              -- ^ time delay
      -> a              -- ^ initial value
      -> SignalBase t a -- ^ input signal
      -> SignalBase t a -- ^ output signal

delay t v = MoC.delay (unit (t, v))

-- | Similar to the previous, but this is the raw instantiation of the
-- 'ForSyDe.Atom.MoC.delay' pattern from "ForSyDe.Atom.MoC". It "borrows" the first
-- event from one signal and appends it at the head of another signal.
--
-- >>> let s1 = readSignal "{1@(-1), 2@2, 3@6, 4@8, 5@9}" :: Signal Int
-- >>> let s2 = readSignal "{3@2, 4@4, 5@5, 6@8, 7@9}" :: Signal Int
-- >>> delay' s1 s2
-- {1@-1s,3@5s,4@7s,5@8s,6@11s,7@12s}
--
-- <<fig/moc-re-pattern-delayp.png>>
delay' ::(Num t, Ord t)
       => SignalBase t a   -- ^ signal "borrowing" the initial event
       -> SignalBase t a   -- ^ input signal
       -> SignalBase t a   -- ^ output signal

delay' = MoC.delay


-- | The @delay@ process "delays" a signal with one
-- event. Instantiates the 'ForSyDe.Atom.MoC.delay' pattern defined in
-- "ForSyDe.Atom.MoC".
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int
-- >>> delay 3 0 s
-- {0@0s,1@3s,2@5s,3@9s,4@11s,5@12s}
-- 
-- <<fig/moc-re-pattern-delay.png>>
unsafeDelay :: (Num t, Ord t)
            => t          -- ^ time delay
            -> SignalBase t a   -- ^ input signal
            -> SignalBase t a   -- ^ output signal
unsafeDelay t MoC.NullS = error "[MoC.DE.RE] cannot delay a non-existing signal" 
unsafeDelay t s@(RE _ x MoC.:- _) = unit (t,x) -&- s
--------COMB --------

-- | @comb@ processes map combinatorial functions on signals and take
-- care of synchronization between input signals. It instantiates the
-- @comb@ pattern (see 'ForSyDe.Atom.MoC.comb22' defined in
-- "ForSyDe.Atom.MoC").
-- 
-- Constructors: @comb[1-4][1-4]@.
--
-- >>> let s1 = infinite 1
-- >>> let s2 = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int
-- >>> comb11 (+1) s2
-- {2@0s,3@2s,4@6s,5@8s,6@9s}
-- >>> comb22 (\a b-> (a+b,a-b)) s1 s2
-- ({2@0s,3@2s,4@6s,5@8s,6@9s},{0@0s,-1@2s,-2@6s,-3@8s,-4@9s})
--
-- <<fig/moc-re-pattern-comb.png>>
comb22 :: (Num t, Ord t)
       => ([a1] -> [a2] -> ([b1], [b2]))     -- ^ function on values
       -> SignalBase t a1                    -- ^ first input signal
       -> SignalBase t a2                    -- ^ second input signal
       -> (SignalBase t b1, SignalBase t b2) -- ^ two output signals
comb11 :: (Ord t, Num t)
       => ([a1] -> [b1])
       -> SignalBase t a1 -> SignalBase t b1                                
comb12 :: (Num t, Ord t)
       => ([a1] -> ([b1], [b2]))
       -> SignalBase t a1 -> (SignalBase t b1, SignalBase t b2)                          
comb13 :: (Num t, Ord t)
       => ([a1] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      
comb14 :: (Num t, Ord t)
       => ([a1] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)                  
comb21 :: (Ord t, Num t)
       => ([a1] -> [a2] -> [b1])
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t b1                          
comb23 :: (Num t, Ord t)
       => ([a1] -> [a2] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> SignalBase t a2 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                
comb24 :: (Num t, Ord t)
       => ([a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> SignalBase t a2 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)            
comb31 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [b1])
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t b1                    
comb32 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> ([b1], [b2]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> (SignalBase t b1, SignalBase t b2)              
comb33 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)          
comb34 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)     
comb41 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> [b1])
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> SignalBase t b1              
comb42 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> (SignalBase t b1, SignalBase t b2)        
comb43 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)    
comb44 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)

comb11 f s1 =
  (detect s1 -? (\t -> ctxt11 (fromList1 t) () f)
    -*- s1 -*)
comb12 f s1 =
  (detect s1 -? (\t -> ctxt12 (fromList1 t) ((),()) f)
    -*- s1 -*<)
comb13 f s1 =
  (detect s1 -? (\t -> ctxt13 (fromList1 t) ((),(),()) f)
    -*- s1 -*<<)
comb14 f s1 =
  (detect s1 -? (\t -> ctxt14 (fromList1 t) ((),(),(),()) f)
    -*- s1 -*<<<)
comb21 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt21 (fromList2 t) () f)
    -*- s1 -*- s2 -*)
comb22 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt22 (fromList2 t) ((),()) f)
    -*- s1 -*- s2 -*<)
comb23 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt23 (fromList2 t) ((),(),()) f)
    -*- s1 -*- s2 -*<<)
comb24 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt24 (fromList2 t) ((),(),(),()) f)
    -*- s1 -*- s2 -*<<<)
comb31 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt31 (fromList3 t) () f)
    -*- s1 -*- s2 -*- s3 -*)
comb32 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt32 (fromList3 t) ((),()) f)
    -*- s1 -*- s2 -*- s3 -*<)
comb33 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt33 (fromList3 t) ((),(),()) f)
    -*- s1 -*- s2 -*- s3 -*<<)
comb34 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt34 (fromList3 t) ((),(),(),()) f)
    -*- s1 -*- s2 -*- s3 -*<<<)
comb41 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt41 (fromList4 t) () f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*)
comb42 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt42 (fromList4 t) ((),()) f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*<)
comb43 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt43 (fromList4 t) ((),(),()) f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*<<)
comb44 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt44 (fromList4 t) ((),(),(),()) f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*<<<)

------- CONSTANT -------

-- | A signal generator which keeps a value constant. As compared with
-- the 'ForSyDe.Atom.MoC.SY.SY', it just constructs an infinite signal
-- with constant value (i.e. a signal with one event starting from
-- time 0).
--
-- Constructors: @constant[1-4]@.
--
-- >>> constant1 2
-- {2@0s}
--
-- <<fig/moc-re-pattern-constant.png>>
constant2 :: (Num t, Ord t)
    => (b1, b2)         -- ^ values to be repeated
          -> (SignalBase t b1, SignalBase t b2) -- ^ generated signals
constant1 :: (Num t, Ord t)
    => b1 -> SignalBase t b1                                
constant3 :: (Num t, Ord t)
    => (b1, b2, b3)
          -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)
constant4 :: (Num t, Ord t)
    => (b1, b2, b3, b4)
          -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)

constant1 = infinite
constant2 = ($$) (infinite,infinite)
constant3 = ($$$) (infinite,infinite,infinite)
constant4 = ($$$$) (infinite,infinite,infinite,infinite)

------- GENERATE -------

generate1 :: (Num t, Ord t)
          =>  ([b1] -> [b1]) -> (t, b1)
          -> SignalBase t b1                                
-- | A signal generator based on a function and a kernel value. It is
-- actually an instantiation of the @stated0X@ constructor (check
-- 'ForSyDe.Atom.MoC.stated22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @generate[1-4]@.
--
-- >>> let (s1,s2) = generate2 (\a b -> (a+1,b+2)) ((3,1),(1,2))
-- >>> takeS 5 s1
-- {1@0s,2@3s,2@4s,2@5s,3@6s}
-- >>> takeS 7 s2
-- {2@0s,4@1s,6@2s,8@3s,10@4s,12@5s,14@6s}
--
-- <<fig/moc-re-pattern-generate.png>>
generate2 :: (Num t, Ord t)
          =>  ([b1] -> [b2] -> ([b1], [b2]))
          -- ^ function to generate next value
          -> ((t, b1), (t, b2))
          -- ^ kernel values tupled with their generation rate.
          -> (SignalBase t b1, SignalBase t b2) -- ^ generated signals
generate3 :: (Num t, Ord t)
          =>  ([b1] -> [b2] -> [b3] -> ([b1], [b2], [b3]))
          -> ((t, b1), (t, b2), (t, b3))
          -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      

generate1 ns i = st 
  where st     = unit i -&>- comb11 ns st 
generate2 ns (i1,i2)
  = let (ns1,ns2) = comb22 ns st1 st2
        (st1,st2) = (unit i1 -&>- ns1, unit i2 -&>- ns2)
    in  (st1,st2)
generate3 ns (i1,i2,i3)
  = let (ns1,ns2,ns3) = comb33 ns st1 st2 st3
        (st1,st2,st3) = (unit i1 -&>- ns1, unit i2 -&>- ns2, unit i3 -&>- ns3)
    in  (st1,st2,st3)
generate4 ns (i1,i2,i3,i4)
  = let (ns1,ns2,ns3,ns4) = comb44 ns st1 st2 st3 st4
        (st1,st2,st3,st4) = (unit i1 -&>- ns1, unit i2 -&>- ns2,
                             unit i3 -&>- ns3, unit i4 -&>- ns4)
    in  (st1,st2,st3,st4)

------- STATED -------

b4s1 :: (Num t, Ord t) => SignalBase t a -> SignalBase t [a]
b4s1 = comb11 (:[])
b4s2 s1 s2 = syncAndFill2 ([],[]) (b4s1 s1) (b4s1 s2)
b4s3 s1 s2 s3 = syncAndFill3 ([],[],[]) (b4s1 s1) (b4s1 s2) (b4s1 s3)

stated11 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> b1)
         -> b1
         -> SignalBase t a1
         -> SignalBase t b1 
stated12 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> (b1, b2))
         -> (b1, b2)
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2) 
stated13 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> (b1, b2, b3))
         -> (b1, b2, b3)
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 
stated21 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> b1)
         -> b1
         -> SignalBase t a1 -> SignalBase t a2
         -> SignalBase t b1 
-- | @state@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.stated22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @stated[1-4][1-4]@.
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: Signal Int
-- >>> takeS 7 $ stated11 (+) (6,1) s
-- {1@0s,2@6s,3@8s,5@12s,7@14s,8@15s,10@18s}
--
-- <<fig/moc-re-pattern-state.png>>
stated22 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> [a2] -> (b1, b2))
         -- ^ next state function
         -> (b1, b2)
         -- ^ initial state values
         -> SignalBase t a1
         -- ^ first input signal
         -> SignalBase t a2
         -- ^ second input signal
         -> (SignalBase t b1, SignalBase t b2) -- ^ output signals
stated23 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> [a2] -> (b1, b2, b3))
         -> (b1, b2, b3)
         -> SignalBase t a1 -> SignalBase t a2
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 
stated31 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> [a3] -> b1)
         -> b1
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> SignalBase t b1 
stated32 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> [a2] -> [a3] -> (b1, b2))
         -> (b1, b2)
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> (SignalBase t b1, SignalBase t b2) 
stated33 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> [a2] -> [a3] -> (b1, b2, b3))
         -> (b1, b2, b3)
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 

stated11 ns i s1       = embedSY11 (SYC.stated11 ns i) (b4s1 s1)
stated21 ns i s1 s2    = embedSY21 (SYC.stated21 ns i) >< b4s2 s1 s2
stated31 ns i s1 s2 s3 = embedSY31 (SYC.stated31 ns i) ><< b4s3 s1 s2 s3
stated12 ns i s1       = embedSY12 (SYC.stated12 ns i) (b4s1 s1)
stated22 ns i s1 s2    = embedSY22 (SYC.stated22 ns i) >< b4s2 s1 s2
stated32 ns i s1 s2 s3 = embedSY32 (SYC.stated32 ns i) ><< b4s3 s1 s2 s3
stated13 ns i s1       = embedSY13 (SYC.stated13 ns i) (b4s1 s1)
stated23 ns i s1 s2    = embedSY23 (SYC.stated23 ns i) >< b4s2 s1 s2
stated33 ns i s1 s2 s3 = embedSY33 (SYC.stated33 ns i) ><< b4s3 s1 s2 s3

------- STATE -------


state11 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> b1)
         -> b1
         -> SignalBase t a1
         -> SignalBase t b1 
state12 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> (b1, b2))
         -> (b1, b2)
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2) 
state13 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> (b1, b2, b3))
         -> (b1, b2, b3)
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 
state21 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> b1)
         -> b1
         -> SignalBase t a1 -> SignalBase t a2
         -> SignalBase t b1 
-- | @state@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.state22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @state[1-4][1-4]@.
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: Signal Int  
-- >>> takeS 7 $ state11 (+) (6,1) s
-- {1@0s,2@6s,3@8s,5@12s,7@14s,8@15s,10@18s}
--
-- <<fig/moc-re-pattern-state.png>>
state22 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> [a2] -> (b1, b2))
         -- ^ next state function
         -> (b1, b2)
         -- ^ initial state values
         -> SignalBase t a1
         -- ^ first input signal
         -> SignalBase t a2
         -- ^ second input signal
         -> (SignalBase t b1, SignalBase t b2) -- ^ output signals
state23 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> [a2] -> (b1, b2, b3))
         -> (b1, b2, b3)
         -> SignalBase t a1 -> SignalBase t a2
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 
state31 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> [a3] -> b1)
         -> b1
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> SignalBase t b1 
state32 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> [a2] -> [a3] -> (b1, b2))
         -> (b1, b2)
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> (SignalBase t b1, SignalBase t b2) 
state33 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> [a2] -> [a3] -> (b1, b2, b3))
         -> (b1, b2, b3)
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 

state11 ns i s1       = embedSY11 (SYC.state11 ns i) (b4s1 s1)
state21 ns i s1 s2    = embedSY21 (SYC.state21 ns i) >< b4s2 s1 s2
state31 ns i s1 s2 s3 = embedSY31 (SYC.state31 ns i) ><< b4s3 s1 s2 s3
state12 ns i s1       = embedSY12 (SYC.state12 ns i) (b4s1 s1)
state22 ns i s1 s2    = embedSY22 (SYC.state22 ns i) >< b4s2 s1 s2
state32 ns i s1 s2 s3 = embedSY32 (SYC.state32 ns i) ><< b4s3 s1 s2 s3
state13 ns i s1       = embedSY13 (SYC.state13 ns i) (b4s1 s1)
state23 ns i s1 s2    = embedSY23 (SYC.state23 ns i) >< b4s2 s1 s2
state33 ns i s1 s2 s3 = embedSY33 (SYC.state33 ns i) ><< b4s3 s1 s2 s3

------- MOORE -------

-- | @moore@ processes model Moore state machines. It is an
-- instantiation of the @moore@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.moore22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @moore[1-4][1-4]@
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: Signal Int  
-- >>> takeS 7 $ moore11 (+) (+1) (6,1) s
-- {2@0s,3@6s,4@8s,6@12s,8@14s,9@15s,11@18s}
--
-- <<fig/moc-re-pattern-moore.png>>          
moore22 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -- ^ next state function
        -> (st -> ([b1], [b2]))
        -- ^ output decoder
        -> st
        -- ^ initial state: tag and value
        -> SignalBase t a1 -> SignalBase t a2 -> (SignalBase t b1, SignalBase t b2)
moore11 :: (Num t, Ord t)
        =>  (st -> [a1] -> st)
        -> (st -> [b1])
        -> st
        -> SignalBase t a1
        -> SignalBase t b1                                
moore12 :: (Num t, Ord t)
        =>  (st -> [a1] -> st)
        -> (st -> ([b1], [b2]))
        -> st
        -> SignalBase t a1
        -> (SignalBase t b1, SignalBase t b2)                          
moore13 :: (Num t, Ord t)
        =>  (st -> [a1] -> st)
        -> (st -> ([b1], [b2], [b3]))
        -> st
        -> SignalBase t a1
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      
moore21 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -> (st -> [b1])
        -> st
        -> SignalBase t a1 -> SignalBase t a2
        -> SignalBase t b1                          
moore23 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -> (st -> ([b1], [b2], [b3]))
        -> st
        -> SignalBase t a1 -> SignalBase t a2
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                
moore31 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st)
        -> (st -> [b1])
        -> st
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> SignalBase t b1                    
moore32 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st)
        -> (st -> ([b1], [b2]))
        -> st
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> (SignalBase t b1, SignalBase t b2)              
moore33 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st)
        -> (st -> ([b1], [b2], [b3]))
        -> st
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)          

moore11 ns od i s1       = comb11 (li1 od) $ stated11 ns i s1
moore21 ns od i s1 s2    = comb11 (li1 od) $ stated21 ns i s1 s2
moore31 ns od i s1 s2 s3 = comb11 (li1 od) $ stated31 ns i s1 s2 s3
moore12 ns od i s1       = comb12 (li1 od) $ stated11 ns i s1 
moore22 ns od i s1 s2    = comb12 (li1 od) $ stated21 ns i s1 s2
moore32 ns od i s1 s2 s3 = comb12 (li1 od) $ stated31 ns i s1 s2 s3
moore13 ns od i s1       = comb13 (li1 od) $ stated11 ns i s1
moore23 ns od i s1 s2    = comb13 (li1 od) $ stated21 ns i s1 s2
moore33 ns od i s1 s2 s3 = comb13 (li1 od) $ stated31 ns i s1 s2 s3

------- MEALY -------

-- | @mealy@ processes model Mealy state machines. It is an
-- instantiation of the @mealy@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.mealy22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @mealy[1-4][1-4]@
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int  
-- >>> takeS 7 $ mealy11 (+) (-) (6,1) s
-- {0@0s,-1@2s,-1@6s,-1@8s,-2@9s,0@12s,2@14s}
--
-- <<fig/moc-re-pattern-mealy.png>>
mealy22 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -- ^ next state function
        -> (st -> [a1] -> [a2] -> ([b1], [b2]))
        -- ^ outpt decoder
        -> st
        -- ^ initial state: tag and value
        -> SignalBase t a1 -> SignalBase t a2
        -> (SignalBase t b1, SignalBase t b2)
mealy11 :: (Num t, Ord t)
        =>  (st -> [a1] -> st) 
        -> (st -> [a1] -> [b1]) 
        -> st
        -> SignalBase t a1
        -> SignalBase t b1                                
mealy12 :: (Num t, Ord t)
        =>  (st -> [a1] -> st) 
        -> (st -> [a1] -> ([b1], [b2])) 
        -> st
        -> SignalBase t a1 
        -> (SignalBase t b1, SignalBase t b2)                          
mealy13 :: (Num t, Ord t)
        =>  (st -> [a1] -> st) 
        -> (st -> [a1] -> ([b1], [b2], [b3])) 
        -> st
        -> SignalBase t a1 
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      
mealy21 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st) 
        -> (st -> [a1] -> [a2] -> [b1]) 
        -> st
        -> SignalBase t a1 -> SignalBase t a2
        -> SignalBase t b1                          
mealy23 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st) 
        -> (st -> [a1] -> [a2] -> ([b1], [b2], [b3])) 
        -> st
        -> SignalBase t a1 -> SignalBase t a2 
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                
mealy31 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st) 
        -> (st -> [a1] -> [a2] -> [a3] -> [b1]) 
        -> st
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> SignalBase t b1  
mealy32 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st) 
        -> (st -> [a1] -> [a2] -> [a3] -> ([b1], [b2])) 
        -> st
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 
        -> (SignalBase t b1, SignalBase t b2)              
mealy33 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st) 
        -> (st -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3])) 
        -> st
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)          
mealy11 ns od i s1       = comb21 (li1 od) (stated11 ns i s1) s1
mealy21 ns od i s1 s2    = comb31 (li1 od) (stated21 ns i s1 s2) s1 s2
mealy31 ns od i s1 s2 s3 = comb41 (li1 od) (stated31 ns i s1 s2 s3) s1 s2 s3
mealy12 ns od i s1       = comb22 (li1 od) (stated11 ns i s1) s1
mealy22 ns od i s1 s2    = comb32 (li1 od) (stated21 ns i s1 s2) s1 s2
mealy32 ns od i s1 s2 s3 = comb42 (li1 od) (stated31 ns i s1 s2 s3) s1 s2 s3
mealy13 ns od i s1       = comb23 (li1 od) (stated11 ns i s1) s1
mealy23 ns od i s1 s2    = comb33 (li1 od) (stated21 ns i s1 s2) s1 s2
mealy33 ns od i s1 s2 s3 = comb43 (li1 od) (stated31 ns i s1 s2 s3) s1 s2 s3


------------------------------

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.RE.RE' signal(s) an
-- strips off their explicit tags, outputting the equivalent
-- 'ForSyDe.Atom.MoC.SY.SY' signal(s), tupled with an SY signal
-- carrying the timestamps for the synchronization points.
--
-- Constructors: @toSY[1-4]@
--
-- >>> let s1 = DE.infinite 1
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: RE.SignalBase t Int
-- >>> toSY2 s1 s2
-- ({0s,2s,6s,8s,9s},{1,1,1,1,1},{1,2,3,4,5})
--
-- <<fig/moc-de-tosy.png>>
toSYC2 :: (Num t, Ord t)
       => RE.SignalBase t a             -- ^ first input DE signal
       -> RE.SignalBase t b             -- ^ second input DE signal
       -> (SY.Signal t, SYC.Signal a, SYC.Signal b)
       -- ^ signal carrying timestamps tupled with the two output
       -- 'ForSyDe.Atom.MoC.SYC.SY' signals
toSYC1  :: (Num t, Ord t)
        => RE.SignalBase t a
        -> (SY.Signal t, SYC.Signal a)
toSYC3 :: (Num t, Ord t)
       => RE.SignalBase t a -> RE.SignalBase t b -> RE.SignalBase t c
       -> (SY.Signal t, SYC.Signal a, SYC.Signal b, SYC.Signal c)
toSYC4 :: (Num t, Ord t)
       => RE.SignalBase t a -> RE.SignalBase t b -> RE.SignalBase t c
       -> RE.SignalBase t d
       -> (SY.Signal t, SYC.Signal a, SYC.Signal b, SYC.Signal c, SYC.Signal d)

splitTs s = ((\(RE.RE t a) -> (SY.SY t, SY.SY a)) <$> s |<)
toSYC1 = splitTs . (fmap . fmap) AE.Prst
toSYC2 s1 s2
  = let (sy1,sy2) = (splitTs,splitTs) $$ comb22 syncf s1 s2
    in  (fst,snd,snd) $$$ (sy1,sy1,sy2)
  where syncf []  []  = ([AE.Abst],   [AE.Abst]  )
        syncf [x] []  = ([AE.Prst x], [AE.Abst]  )
        syncf []  [x] = ([AE.Abst],   [AE.Prst x])
        syncf [x] [y] = ([AE.Prst x], [AE.Prst y])
toSYC3 s1 s2 s3
  = let (sy1,sy2,sy3) = (splitTs,splitTs,splitTs) $$$ comb33 syncf s1 s2 s3
    in  (fst,snd,snd,snd) $$$$ (sy1,sy1,sy2,sy3)  
  where syncf x y z  = (lToA x, lToA y, lToA z)
        lToA []  = [AE.Abst]
        lToA [x] = [AE.Prst x]
toSYC4 s1 s2 s3 s4  
  = let (sy1,sy2,sy3,sy4) = (splitTs,splitTs,splitTs,splitTs)
                            $$$$ comb44 syncf s1 s2 s3 s4
    in  (fst,snd,snd,snd,snd) $$$$$ (sy1,sy1,sy2,sy3,sy4) 
  where syncf x y z a = (lToA x, lToA y, lToA z, lToA a)
        lToA []  = [AE.Abst]
        lToA [x] = [AE.Prst x]

-- | Wraps explicit timestamps to a (set of) 'ForSyDe.Atom.MoC.SY.SY'
-- signal(s), rendering the equivalent synchronized
-- 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- __OBS:__ cleaning behavior!
--
-- Constructors: @fromSY@, @fromSY2@, @fromSY3@, @fromSY4@.
--
-- >>> let s1 = SY.signal [0,3,4,6,9]
-- >>> let s2 = SY.signal [1,2,3,4,5]
-- >>> fromSY s1 s2
-- {1@0s,2@3s,3@4s,4@6s,5@9s}
--
-- <<fig/moc-sy-tode.png>>
fromSYC2 :: (Num t, Ord t)
         => SY.Signal t
         -- ^ SY signal carrying 'ForSyDe.Atom.MoC.DE.DE' timestamps
         -> SYC.Signal a                -- ^ first input SYC signal
         -> SYC.Signal b                -- ^ second input SYC signal
         -> (RE.SignalBase t a, RE.SignalBase t b)
         -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals
fromSYC1 :: (Num t, Ord t)
         => SY.Signal t -> SYC.Signal a
         -> RE.SignalBase t a
fromSYC3 :: (Num t, Ord t)
         => SY.Signal t -> SYC.Signal a -> SYC.Signal b -> SYC.Signal c 
         -> (RE.SignalBase t a, RE.SignalBase t b, RE.SignalBase t c)
fromSYC4 :: (Num t, Ord t)
  => SY.Signal t -> SYC.Signal a -> SYC.Signal b -> SYC.Signal c -> SYC.Signal d 
  -> (RE.SignalBase t a, RE.SignalBase t b, RE.SignalBase t c, RE.SignalBase t d)
         
eventToDE (SY.SY t) (SY.SY a) = RE.RE t a
cleanAbst [AE.Prst x] = [x]
cleanAbst [AE.Abst]   = []

fromSYC1 ts s1          = comb11 cleanAbst $ eventToDE <$> ts <*> s1
fromSYC2 ts s1 s2       = (fromSYC1 ts s1, fromSYC1 ts s2)
fromSYC3 ts s1 s2 s3    = (fromSYC1 ts s1, fromSYC1 ts s2, fromSYC1 ts s3)
fromSYC4 ts s1 s2 s3 s4 = (fromSYC1 ts s1, fromSYC1 ts s2, fromSYC1 ts s3,
                           fromSYC1 ts s4)

reToDE (RE.RE t a) = (DE.DE t a)
toDE1 s1 = fmap reToDE s1
toDE2 s1 s2 = (toDE1 s1, toDE1 s2)
toDE3 s1 s2 s3 = (toDE1 s1, toDE1 s2, toDE1 s3)
toDE4 s1 s2 s3 s4 = (toDE1 s1, toDE1 s2, toDE1 s3, toDE1 s4)

deToRE (DE.DE t a) = (RE.RE t a)
fromDE1 s1 = fmap deToRE s1
fromDE2 s1 s2 = (fromDE1 s1, fromDE1 s2)
fromDE3 s1 s2 s3 = (fromDE1 s1, fromDE1 s2, fromDE1 s3)
fromDE4 s1 s2 s3 s4 = (fromDE1 s1, fromDE1 s2, fromDE1 s3, fromDE1 s4)

------- HYBRID PROCESSES -------

-- | Embeds a 'ForSyDe.Atom.MoC.SY.SY' process inside a
-- 'ForSyDe.Atom.MoC.DE.DE' environment. Internally, it synchronizes
-- the input signals, translates them to SY, feeds them to a SY
-- process and translates the result back to DE using the same input
-- tags. Seen from outside, this process behaves like a DE process
-- with "instantaneous response", even for feedback loops.
--
-- Constructors: @embedSY[1-4][1-4]@.
--
-- For the following example, see the difference between its output
-- and the one of 'ForSyDe.Atom.MoC.DE.stated22'
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: DE.Signal Int
-- >>> embedSY11 (SY.stated11 (+) 1) s
-- {1@0s,2@2s,4@6s,7@8s,11@9s}
--
-- <<fig/moc-de-pattern-embedsy.png>>
embedSY22 :: (Num t, Ord t)
          => (SYC.Signal a1 -> SYC.Signal a2
              -> (SYC.Signal b1, SYC.Signal b2))
          -- ^ 'ForSyDe.Atom.MoC.SY.SY' process
          -> RE.SignalBase t a1 -- ^ first input DE signal
          -> RE.SignalBase t a2 -- ^ second input DE signal 
          -> (RE.SignalBase t b1, RE.SignalBase t b2)
          -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals

embedSY11 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC1 ts $     syproc sy1 
embedSY12 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC2 ts ><   syproc sy1
embedSY13 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC3 ts ><<  syproc sy1
embedSY14 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC4 ts ><<< syproc sy1

embedSY21 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC1 ts $     syproc sy1 sy2
embedSY22 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC2 ts ><   syproc sy1 sy2
embedSY23 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC3 ts ><<  syproc sy1 sy2
embedSY24 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC4 ts ><<< syproc sy1 sy2

embedSY31 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC1 ts $     syproc sy1 sy2 sy3
embedSY32 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC2 ts ><   syproc sy1 sy2 sy3
embedSY33 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC3 ts ><<  syproc sy1 sy2 sy3
embedSY34 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC4 ts ><<< syproc sy1 sy2 sy3

embedSY41 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC1 ts $     syproc sy1 sy2 sy3 sy4
embedSY42 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC2 ts ><   syproc sy1 sy2 sy3 sy4
embedSY43 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC3 ts ><<  syproc sy1 sy2 sy3 sy4
embedSY44 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC4 ts ><<< syproc sy1 sy2 sy3 sy4


----------------------------------------------------

syncAndHold2 (i1,i2)
  = embedSY22 (\s1 s2 ->
                 (SYC.current i1 s1, SYC.current i2 s2))
syncAndHold3 (i1,i2,i3)
  = embedSY33 (\s1 s2 s3 ->
                 (SYC.current i1 s1, SYC.current i2 s2,
                   SYC.current i3 s3))
syncAndHold4 (i1,i2,i3,i4)
  = embedSY44 (\s1 s2 s3 s4 ->
                 (SYC.current i1 s1, SYC.current i2 s2,
                   SYC.current i3 s3, SYC.current i4 s4))

syncAndFill2 (i1,i2)
  = embedSY22 (\s1 s2 ->
                 (SYC.fill i1 s1, SYC.fill i2 s2))
syncAndFill3 (i1,i2,i3)
  = embedSY33 (\s1 s2 s3 ->
                 (SYC.fill i1 s1, SYC.fill i2 s2,
                   SYC.fill i3 s3))
syncAndFill4 (i1,i2,i3,i4)
  = embedSY44 (\s1 s2 s3 s4 ->
                 (SYC.fill i1 s1, SYC.fill i2 s2,
                   SYC.fill i3 s3, SYC.fill i4 s4))
