{-# OPTIONS_HADDOCK prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SDF
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @SDF@ library implements the execution semantics for the atoms
-- operating according to the synchronous dataflow model of
-- computation. This module also provides a set of helpers for
-- instantiating the MoC layer patterns described in the
-- "ForSyDe.Atom.MoC" module as meaningful synchronous dataflow
-- process constructors.
--
-- For an overview about atoms, layers and patterns, please refer to
-- the "ForSyDe.Atom" module documentation, and for an overview of the
-- MoC layer entities refer to <ForSyDe-Atom.html#g:3 the MoC layer section>.
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
----------------------------------------------------------------------

module ForSyDe.Atom.MoC.SDF (
  
  -- * Synchronous data flow (@SDF@) event

  -- | The synchronous data flow (@SDF@) is an untimed MoC. On untimed
  -- MoCs, <ForSyDe-Atom.html#lee98 [Lee98]> states that: "when tags
  -- are partially ordered rather than totally ordered, we say that
  -- the system is untimed. Untimed systems cannot have the same
  -- notion of causality as timed systems [see
  -- 'ForSyDe.Atom.MoC.SY.SY']. (...)  Processes defined in terms of
  -- constraints on the tags in the signals (...) have a /consistent
  -- cut/ rather than /simultaneity/."  Regarding SDF, it states that
  -- "is a special case of Kahn process networks
  -- <ForSyDe-Atom.html#kahn76 [Kahn76]>. A dataflow process is a Kahn
  -- process that is also sequential, where the events on the
  -- self-loop signal denote the firings of the dataflow actor. The
  -- firing rules of a dataflow actor are partial ordering constraints
  -- between these events and events on the inputs. (...)
  -- Produced/consumed events are defined in terms of relations with
  -- the events in the firing signal. It results that for the same
  -- firing \(i\), \(e_i<e_o\), as an intuitive sort of causality
  -- constraint."
  --
  -- Based on the above insights, we can formulate a simplified
  -- definition of the ForSyDe-Atom implementation of SDF:
  --
  -- [The SDF MoC] is abstracting the execution semantics of a
  --   dataflow system where computation is performed according to
  --   firing rules where the production and the consumption rates are
  --   fixed.
  --
  -- Below is a /possible/ behavior in time of the input and the
  -- output signals of a SDF process. Events sharing the same partial
  -- ordering in relation to one firing are underlined:
  --
  -- <<fig/moc-sdf-example.png>>
  --
  -- Implementing the SDF tag system implied a series of engineering
  -- decisions which lead to the following particularities:
  --
  -- 1. signals represent FIFO channels, and tags are implicit from
  --    their position in the 'ForSyDe.Atom.MoC.Stream.Stream'
  --    structure. Internally, 'ForSyDe.Atom.MoC.SDF.SDF' signals have
  --    exactly the same structure as 'ForSyDe.Atom.MoC.SY.SY'
  --    signals, whereas the partial ordering is imposed by the
  --    processes alone.
  --
  -- 1. the 'ForSyDe.Atom.MoC.SDF.SDF' event constructor wraps only a
  --    value.
  --
  -- 1. being an /untimed MoC/, the order between events is partial to
  --    the firings of processes. An SDF atom will fire only when
  --    there are enough events to trigger its inputs. Once a firing
  --    occurs, it will take care of partitioning the input or output
  --    signals.
  --
  -- 1. SDF atoms require a context for executing the passed
  --    functions: the consumption \(c\) and production \(p\) rates
  --    need to be known in order to determine the behavior of a
  --    process. Also, the type signature for the functions passed as
  --    arguments need to reflect the fact that multiple events are
  --    handled during a firing.
  --
  -- 1. the previous statement can be synthesized into the following
  --    formal definition for the SDF <ForSyDe-Atom-MoC.html#context execution context>:
  --
  -- <<fig/eqs-moc-sdf-context.png>>
  --
  -- To see how ForSyDe-Atom implements the above definition in
  -- practice, check the <#i:SDF type family instance> for the
  -- 'ForSyDe.Atom.MoC.Fun' and 'ForSyDe.Atom.MoC.Ret' types.
  
  SDF(..),
  
  -- * Aliases & utilities

  -- | These are type synonyms and utilities provided for user
  -- convenience. They mainly concern the construction and usage of
  -- signals.

  Signal, Prod, Cons, signal, readSignal,

  -- * Process constructors
  
  -- | These SDF process constructors are basically specific
  -- instantiations of the patterns of atoms defined in
  -- "ForSyDe.Atom.MoC". 

  -- ** Simple

  delay, delay', 
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  reconfig11, reconfig12, reconfig13, reconfig14,
  reconfig21, reconfig22, reconfig23, reconfig24,
  reconfig31, reconfig32, reconfig33, reconfig34,
  reconfig41, reconfig42, reconfig43, reconfig44,
  
  constant1, constant2, constant3, constant4,

  generate1, generate2, generate3, generate4,

  stated11, stated12, stated13, stated14,
  stated21, stated22, stated23, stated24,
  stated31, stated32, stated33, stated34,
  stated41, stated42, stated43, stated44,

  state11, state12, state13, state14,
  state21, state22, state23, state24,
  state31, state32, state33, state34,
  state41, state42, state43, state44,

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,

  -- ** Interfaces

  toSY1, toSY2, toSY3, toSY4,
  zipx, unzipx  

  ) where

import ForSyDe.Atom.MoC.SDF.Core
import ForSyDe.Atom.MoC.SDF.Lib
import ForSyDe.Atom.MoC.SDF.Interface
