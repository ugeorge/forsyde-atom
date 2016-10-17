{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton.Vector.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The library for the 'Vector' type. Contains the main skeletons.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector.Lib where

import Data.Maybe
import ForSyDe.Atom.Skeleton
import ForSyDe.Atom.Utility
import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)

import ForSyDe.Atom.Skeleton.Vector.Core

---------------
-- Skeletons --
---------------

map  :: (a -> b) -> Vector a -> Vector b
red  :: (a -> a -> a) -> Vector a -> a
pipe :: Vector (a -> a) -> a -> a
scan :: Vector (a -> a) -> a -> Vector a
map  = (=$=)
red  = (=\=)
pipe = (=<<=)
scan  ps s = map (=<<= s) (inits ps)
scan' ps s = map (=<<= s) (inits $ unit id <++> ps)

map11 p v1                      = (p =$= v1)
map21 p v1 v2                   = (p =$= v1 =*= v2)
map31 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3)
map41 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4)
map51 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5)
map61 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6)
map71 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7)
map81 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 =*= v8)

map12 p v1                      = (p =$= v1 |<)
map22 p v1 v2                   = (p =$= v1 =*= v2 |<)
map32 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3 |<)
map42 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4 |<)
map52 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<)
map62 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<)
map72 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<)
map82 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v5 =*= v8 |<)

map13 p v1                      = (p =$= v1 |<<)
map23 p v1 v2                   = (p =$= v1 =*= v2 |<<)
map33 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3 |<<)
map43 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4 |<<)
map53 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<<)
map63 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<<)
map73 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<<)
map83 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v5 =*= v8 |<<)

map14 p v1                      = (p =$= v1 |<<<)
map24 p v1 v2                   = (p =$= v1 =*= v2 |<<<)
map34 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3 |<<<)
map44 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4 |<<<)
map54 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<<<)
map64 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<<<)
map74 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<<<)
map84 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 =*= v8 |<<<)

reduce1 p v1                      = p =\= v1
reduce2 p v1 v2                   = map21 p v1 (tail v2) =<<= first v2
reduce3 p v1 v2 v3                = map31 p v1 v2 (tail v3) =<<= first v3
reduce4 p v1 v2 v3 v4             = map41 p v1 v2 v3 (tail v4) =<<= first v4
reduce5 p v1 v2 v3 v4 v5          = map51 p v1 v2 v3 v4 (tail v5) =<<= first v5
reduce6 p v1 v2 v3 v4 v5 v6       = map61 p v1 v2 v3 v4 v5 (tail v6) =<<= first v6
reduce7 p v1 v2 v3 v4 v5 v6 v7    = map71 p v1 v2 v3 v4 v5 v6 (tail v7) =<<= first v7
reduce8 p v1 v2 v3 v4 v5 v6 v7 v8 = map81 p v1 v2 v3 v4 v5 v6 v7 (tail v8) =<<= first v8

reduce1' p i v1                      = p =\= v1 <: i 
reduce2' p i v1 v2                   = map21 p v1 v2 =<<= i
reduce3' p i v1 v2 v3                = map31 p v1 v2 v3 =<<= i
reduce4' p i v1 v2 v3 v4             = map41 p v1 v2 v3 v4 =<<= i
reduce5' p i v1 v2 v3 v4 v5          = map51 p v1 v2 v3 v4 v5 =<<= i
reduce6' p i v1 v2 v3 v4 v5 v6       = map61 p v1 v2 v3 v4 v5 v6 =<<= i
reduce7' p i v1 v2 v3 v4 v5 v6 v7    = map71 p v1 v2 v3 v4 v5 v6 v7 =<<= i
reduce8' p i v1 v2 v3 v4 v5 v6 v7 v8 = map81 p v1 v2 v3 v4 v5 v6 v7 v8 =<<= i

prefix1 p                   = map11 (reduce1 p) . inits
prefix2 p v1 v2             = map21 (reduce2 p) (unit v1) (inits v2)
prefix3 p v1 v2 v3          = map31 (reduce3 p) (unit v1) (unit v2) (inits v3)
prefix4 p v1 v2 v3 v4       = map41 (reduce4 p) (unit v1) (unit v2) (unit v3) (inits v4)
prefix5 p v1 v2 v3 v4 v5    = map51 (reduce5 p) (unit v1) (unit v2) (unit v3) (unit v4) (inits v5)
prefix6 p v1 v2 v3 v4 v5 v6 = map61 (reduce6 p) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (inits v6)

prefix1' p i                   = map11 (reduce1' p i) . inits
prefix2' p i v1 v2             = map21 (reduce2' p i) (unit v1) (inits v2)
prefix3' p i v1 v2 v3          = map31 (reduce3' p i) (unit v1) (unit v2) (inits v3)
prefix4' p i v1 v2 v3 v4       = map41 (reduce4' p i) (unit v1) (unit v2) (unit v3) (inits v4)
prefix5' p i v1 v2 v3 v4 v5    = map51 (reduce5' p i) (unit v1) (unit v2) (unit v3) (unit v4) (inits v5)
prefix6' p i v1 v2 v3 v4 v5 v6 = map61 (reduce6' p i) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (inits v6)

suffix1 p                   = map11 (reduce1 p) . tails
suffix2 p v1 v2             = map21 (reduce2 p) (unit v1) (tails v2)
suffix3 p v1 v2 v3          = map31 (reduce3 p) (unit v1) (unit v2) (tails v3)
suffix4 p v1 v2 v3 v4       = map41 (reduce4 p) (unit v1) (unit v2) (unit v3) (tails v4)
suffix5 p v1 v2 v3 v4 v5    = map51 (reduce5 p) (unit v1) (unit v2) (unit v3) (unit v4) (tails v5)
suffix6 p v1 v2 v3 v4 v5 v6 = map61 (reduce6 p) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (tails v6)

suffix1' p i                   = map11 (reduce1' p i) . tails
suffix2' p i v1 v2             = map21 (reduce2' p i) (unit v1) (tails v2)
suffix3' p i v1 v2 v3          = map31 (reduce3' p i) (unit v1) (unit v2) (tails v3)
suffix4' p i v1 v2 v3 v4       = map41 (reduce4' p i) (unit v1) (unit v2) (unit v3) (tails v4)
suffix5' p i v1 v2 v3 v4 v5    = map51 (reduce5' p i) (unit v1) (unit v2) (unit v3) (unit v4) (tails v5)
suffix6' p i v1 v2 v3 v4 v5 v6 = map61 (reduce6' p i) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (tails v6)

pipe0                             = pipe
pipe1 p v1 s                      = map11 p v1 `pipe` s
pipe2 p v1 v2 s                   = map21 p v1 v2 `pipe` s
pipe3 p v1 v2 v3 s                = map31 p v1 v2 v3 `pipe` s
pipe4 p v1 v2 v3 v4 s             = map41 p v1 v2 v3 v4 `pipe` s
pipe5 p v1 v2 v3 v4 v5 s          = map51 p v1 v2 v3 v4 v5 `pipe` s
pipe6 p v1 v2 v3 v4 v5 v6 s       = map61 p v1 v2 v3 v4 v5 v6 `pipe` s
pipe7 p v1 v2 v3 v4 v5 v6 v7 s    = map71 p v1 v2 v3 v4 v5 v6 v7 `pipe` s
pipe8 p v1 v2 v3 v4 v5 v6 v7 v8 s = map81 p v1 v2 v3 v4 v5 v6 v7 v8 `pipe` s

systolic0                             = scan
systolic1 p v1 s                      = map11 p v1 `scan` s
systolic2 p v1 v2 s                   = map21 p v1 v2 `scan` s
systolic3 p v1 v2 v3 s                = map31 p v1 v2 v3 `scan` s
systolic4 p v1 v2 v3 v4 s             = map41 p v1 v2 v3 v4 `scan` s
systolic5 p v1 v2 v3 v4 v5 s          = map51 p v1 v2 v3 v4 v5 `scan` s
systolic6 p v1 v2 v3 v4 v5 v6 s       = map61 p v1 v2 v3 v4 v5 v6 `scan` s
systolic7 p v1 v2 v3 v4 v5 v6 v7 s    = map71 p v1 v2 v3 v4 v5 v6 v7 `scan` s
systolic8 p v1 v2 v3 v4 v5 v6 v7 v8 s = map81 p v1 v2 v3 v4 v5 v6 v7 v8 `scan` s

cascade0 p                 vs1 vs2 = map11 (\            s2 s1 -> map11 p             s1 `scan` s2)                 vs2 `pipe` vs1
cascade1 p vv1             vs1 vs2 = map21 (\v1          s2 s1 -> map21 p v1          s1 `scan` s2) vv1             vs2 `pipe` vs1
cascade2 p vv1 vv2         vs1 vs2 = map31 (\v1 v2       s2 s1 -> map31 p v1 v2       s1 `scan` s2) vv1 vv2         vs2 `pipe` vs1
cascade3 p vv1 vv2 vv3     vs1 vs2 = map41 (\v1 v2 v3    s2 s1 -> map41 p v1 v2 v3    s1 `scan` s2) vv1 vv2 vv3     vs2 `pipe` vs1
cascade4 p vv1 vv2 vv3 vv4 vs1 vs2 = map51 (\v1 v2 v3 v4 s2 s1 -> map51 p v1 v2 v3 v4 s1 `scan` s2) vv1 vv2 vv3 vv4 vs2 `pipe` vs1

mesh0 p                 vs1 vs2 = map11 (\            s2 s1 -> map11 p             s1 `scan` s2)                 vs2 `scan` vs1
mesh1 p vv1             vs1 vs2 = map21 (\v1          s2 s1 -> map21 p v1          s1 `scan` s2) vv1             vs2 `scan` vs1
mesh2 p vv1 vv2         vs1 vs2 = map31 (\v1 v2       s2 s1 -> map31 p v1 v2       s1 `scan` s2) vv1 vv2         vs2 `scan` vs1
mesh3 p vv1 vv2 vv3     vs1 vs2 = map41 (\v1 v2 v3    s2 s1 -> map41 p v1 v2 v3    s1 `scan` s2) vv1 vv2 vv3     vs2 `scan` vs1
mesh4 p vv1 vv2 vv3 vv4 vs1 vs2 = map51 (\v1 v2 v3 v4 s2 s1 -> map51 p v1 v2 v3 v4 s1 `scan` s2) vv1 vv2 vv3 vv4 vs2 `scan` vs1

-- Skeletons proven by injectivity (equivalent factorized forms exist)

fanout x = x :> fanout x

fanoutn n x | n == 0    = Null
            | otherwise = x :> fanoutn (n-1) x

tail Null    = error "tail: Vector is empty"
tail (x:>xs) = xs
-- tail      = (<@!> 2) . tails

init Null      = error "init: Vector is empty"
init (_:>Null) = Null
init (v:>vs)   = v :> init vs
-- init      = (<@!> 2) . reverse . inits

shiftr vs v = v :> init vs
shiftl vs v = tail vs <: v
rotl   Null = Null
rotl   vs   = tail vs <: first vs
rotr   Null = Null
rotr   vs   = last vs :> init vs

-- Permutators

last    :: Vector a -> a
first   :: Vector a -> a
reverse :: Vector a -> Vector a
inits   :: Vector a -> Vector (Vector a)
tails   :: Vector a -> Vector (Vector a)
length  :: Vector a -> Int

length   = red (+) . map (\_ -> 1)
concat   = red (<++>)
first    = red (\x y -> x)
last     = red (\x y -> y)
reverse  = red (\x y -> y <++> x)                    . map unit
inits    = red (\x y -> x <++> map (last  x <++>) y) . map (unit . unit)
tails    = red (\x y -> map (<++> first y) x <++> y) . map (unit . unit)
-- filter f = reduce1' (\x y -> if f (first x) then x <++> y else y) Null . map unit
takeWhile f = concat . reduce1 selfunc . map (unit . unit)
  where selfunc x y = if f (first (first y)) && (not . isNull . last) x then x <++> y else x

        
first' Null = Null
first' xs   = first xs
last'  Null = Null
last'  xs   = last xs
init'  Null = Null
init'  xs   = init xs
tail'  Null = Null
tail'  xs   = tail xs

-- Index-based selectors

get     ix  = reduce2' (\i x y -> if i == ix then x else y) Nothing indexes . map Just
take    n   = reduce2' (\i x y -> if i < n then x <++> y else x) Null indexes . map unit
drop    n   = reduce2' (\i x y -> if i > n then x <++> y else y) Null indexes . map unit
filterIdx f = reduce2' (\i x y -> if f i   then x <++> y else y) Null indexes . map unit
replace n r = reduce2' (\i x y -> if i == n then r :> y else x <++> y) Null indexes . map unit
stride  f s = let stridef i x y | i `mod` s == f = x <++> y
                                | otherwise      = y
              in  reduce2' stridef Null indexes . map unit
group   n   = let groupf i x y  | i `mod` n == 0 = x <++> y
                                | otherwise      = (first x <++> first' y) :> tail' y
              in  reduce2' groupf Null indexes . map (unit . unit)


v <@>  ix = get ix v
v <@!> ix = fromJust $ get ix v
odds      = filterIdx odd
evens     = filterIdx even

-- bitrev (x:>Null) = unit x
-- bitrev xs        = bitrev (evens xs) <++> bitrev (odds xs)
-- duals   v = let k = length v `div` 2
--             in  map21 (,) (take k v) (drop k v)
-- unduals v = let (x,y) = (v |<) 
--             in  x <++> y

generate n f i = fanoutn n f `scan` i
iterate  n f i = fanoutn (n-1) f `scan'` i

gather  ix v     =  (=$=)                          (v <@!>) ix
gather2 ix vv    = ((=$=).(=$=))                   (vv <@!>) ix
gather3 ix vvv   = ((=$=).(=$=).(=$=))             (vvv <@!>) ix
gather4 ix vvvv  = ((=$=).(=$=).(=$=).(=$=))       (vvvv <@!>) ix
gather5 ix vvvvv = ((=$=).(=$=).(=$=).(=$=).(=$=)) (vvvvv <@!>) ix

scatter ix hv = reduce2' (\i h r -> replace i (first r) h) hv ix . map unit


-- group n v = map (take n) $ prefix1 dropseries v
--   where dropseries = unit id <++> fanoutn nstages (drop n)
--         nstages    = ceiling $ fromIntegral (length v) / fromIntegral n - 1


----------------- DOCUMENTATION -----------------
-- | @map@ maps a function on a vector (See also: '=$=', '=*=', '|<').
--
-- If the vector contains signals, then the function is, naturally, a
-- process. The example below shows a typical case when the function
-- passed is a /process constructor/ rather than a fully applied
-- process. Using the same powerful applicative mechanism, the process
-- constructor is systematically applied to the first vectors of
-- values, yelding an intermediate vector of processes. This vector of
-- processes is then furher applied to the subsequent vectors of
-- signals, constructing the /data-parallel/ process network depicted
-- below.
--
-- <<includes/figs/skel-map-formula.png>> <<includes/figs/skel-map-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- >  map11, map12, map13, map14,
-- >  map21, map22, map23, map24,
-- >  map31, map32, map33, map34,
-- >  map41, map42, map43, map44,
-- >  map51, map52, map53, map54,
-- >  map61, map62, map63, map64,
-- >  map71, map72, map73, map74,
-- >  map81, map82, map83, map84,
map22 :: (a1 -> a2 -> (b1, b2))
         -- ^ @function22@. if /a/ and /b/ are signals, then this
         -- should be @process22@. See "ForSyDe.Atom.MoC".
         -> Vector a1              -- ^ first input vector
         -> Vector a2              -- ^ second input vector
         -> (Vector b1, Vector b2) -- ^ two output vectors

-- | @red@ reduces a vector to an element based on an associative
-- function. (See also: '=\=', '=<<=', 'map22')
--
-- Again, if the vector that needs to be reduced contains signals,
-- then the function passed as the first argument is a process. As
-- with 'map22', one can systematically build the reduction vector of
-- processes in an applicative manner, by applying a process
-- constructor to a series of vectors until a two-input associative
-- process is obtained. A typical example is depicted below:
--
-- <<includes/figs/skel-red-formula.png>> <<includes/figs/skel-red-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > reduce1, reduce2, reduce3, reduce4, reduce5, re6, reduce7, reduce8,
--
-- __OBS:__ we use the pipe operator '=<<=' which is in fact a special
-- case of the reduce operator '=\=', which allows partial application
-- of functions with an rbitrary number of inputs.
reduce2 :: (a1 -> a2 -> a2 -> a2)
        -- ^ @function31@, which is a constructor for an associative
        -- @function21@ (@a2 -> a2 -> a2@).  @a1@ is obtained from the
        -- first vector, whereas the last two arguments of type @a2@
        -- belong to the second vector, which is being reduced.
        -> Vector a1  -- ^ first input vector
        -> Vector a2  -- ^ second input vector
        -> a2         -- ^ the second input vector reduced to one element

-- | @red'@ is a variant of @red@ which takes a separate initial
-- element. (See also: 'reduce2', '=\=', '=<<=', 'map22')
--
-- For a typical usage example, check the documentation for 'reduce2'.
--
-- <<includes/figs/skel-redp-formula.png>> 
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > reduce1', reduce2', reduce3', reduce4', reduce5', re6', reduce7', reduce8',
--
-- __OBS:__ we use the pipe operator '=<<=' which is in fact a special
-- case of the reduce operator '=\=', which allows partial application
-- of functions with an rbitrary number of inputs.
reduce2' :: (a1 -> a2 -> a2 -> a2)
        -- ^ @function31@, which is a constructor for an associative
        -- @function21@ (@a2 -> a2 -> a2@).  @a1@ is obtained from the
        -- first vector, whereas the last two arguments of type @a2@
        -- belong to the second vector, which is being reduced.
        -> a2         -- ^ initial element, which shall be piped into
                      -- the reduction network.
        -> Vector a1  -- ^ first input vector
        -> Vector a2  -- ^ second input vector
        -> a2         -- ^ the second input vector reduced to one element

-- | @pref@ peforms the /parallel prefix/ operation on a vector. (See
-- also: 'reduce2', 'map22', 'inits')
--
-- In case the last input vector contains signals, then @pref@
-- constructs the following process network, where the process passed
-- as argument is assumed to be fully applied, and to take exactly two
-- signals as input (See 'map22' and 'reduce2' for a more generic
-- example).
--
-- <<includes/figs/skel-pref-formula.png>> <<includes/figs/skel-pref-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > prefix1,  prefix2,  prefix3,  prefix4,  prefix5,  prefix6, 
prefix2 :: (a -> b -> b -> b)
         -- ^ @function31@, which is a constructor for an associative
         -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
         -- first vector, whereas the last two arguments of type @b@
         -- belong to the second vector, which is being reduced.
         -> Vector a  -- ^ first input vector
         -> Vector b  -- ^ second input vector
         -> Vector b  -- ^ parallel prefix on the second input vector 


-- | @prefix'@ is a variant of @prefix@ which takes an initial
-- element. (See also: 'prefix2', 'reduce2', 'map22', 'inits')
--
-- See the example for 'prefix2'.
--
-- <<includes/figs/skel-prefp-formula.png>> 
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > prefix1',  prefix2',  prefix3',  prefix4',  prefix5',  prefix6', 
prefix2' :: (a -> b -> b -> b)
          -- ^ @function31@, which is a constructor for an associative
          -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
          -- first vector, whereas the last two arguments of type @b@
          -- belong to the second vector, which is being reduced.
          -> b         -- ^ initial element, which shall be piped into
                       -- the reduction network.
          -> Vector a  -- ^ first input vector
          -> Vector b  -- ^ second input vector
          -> Vector b  -- ^ parallel prefix on the second input vector 

-- | @suffix@ peforms the /parallel suffix/ operation on a vector. (See
-- also: 'reduce2', 'map22', 'tails')
--
-- In case the last input vector contains signals, then @suffix@
-- constructs the following process network, where the process passed
-- as argument is assumed to be fully applied, and to take exactly two
-- signals as input (See 'map22' and 'reduce2' for a more generic
-- example).
--
-- <<includes/figs/skel-suf-formula.png>> <<includes/figs/skel-suf-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > suffix1,  suffix2,  suffix3,  suffix4,  suffix5,  suffix6, 
suffix2 :: (a -> b -> b -> b)
         -- ^ @function31@, which is a constructor for an associative
         -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
         -- first vector, whereas the last two arguments of type @b@
         -- belong to the second vector, which is being reduced.
         -> Vector a  -- ^ first input vector
         -> Vector b  -- ^ second input vector
         -> Vector b  -- ^ parallel suffix on the second input vector 


-- | @suffix'@ is a variant of @suffix@ which takes an initial
-- element. (See also: 'suffix2', 'reduce2', 'map22', 'tails')
--
-- See the example for 'suffix2'.
--
-- <<includes/figs/skel-sufp-formula.png>> 
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > suffix1',  suffix2',  suffix3',  suffix4',  suffix5',  suffix6', 
suffix2' :: (a -> b -> b -> b)
          -- ^ @function31@, which is a constructor for an associative
          -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
          -- first vector, whereas the last two arguments of type @b@
          -- belong to the second vector, which is being reduced.
          -> b         -- ^ initial element, which shall be piped into
                       -- the reduction network.
          -> Vector a  -- ^ first input vector
          -> Vector b  -- ^ second input vector
          -> Vector b  -- ^ parallel suffix on the second input vector 

-- | @pipe@ creates a pipeline of functions from a vector. While
-- itself is a special case of the reduction operaton '=\=', it
-- constructs a more flexible and robust process network (See also:
-- 'reduce2'', 'map22', '=<<=').
--
-- The following example assumes fully applied processes that take
-- exactly one signal as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-pipe-atom-formula.png>> <<includes/figs/skel-pipe-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below,
-- which (except 'pipe0') are the functional equivalent of 'reduce2''
-- but with flipped arguments. Check the interfaces in @ghci@ to see
-- their usage. To create your own constructors please follow the
-- examples set in the source code.
--
-- > pipe0, pipe1, pipe2, pipe3, pipe4, pipe5, pipe6, pipe7, pipe8,
pipe0 :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> a               -- ^ output 



-- | @systolic@ creates a systolic array of functions from a
-- vector. While itself is a generalization of the parallel prefix
-- 'prefix2', it constructs a more flexible and robust process network
-- (See also: 'prefix2'', 'map22', '=<<=').
--
-- The following example assumes fully applied processes that take
-- exactly one signal as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-systolic-formula.png>> <<includes/figs/skel-systolic-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below,
-- which (except 'systolic0') are the functional equivalent of
-- 'prefix2'' but with flipped arguments. Check the interfaces in
-- @ghci@ to see their usage. To create your own constructors please
-- follow the examples set in the source code.
--
-- > systolic0, systolic1, systolic2, systolic3, systolic4, systolic5, systolic6, systolic7, systolic8,
systolic0 :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> Vector a        -- ^ output 

-- | @cascade@ creates a \"cascading mesh\" as a result of piping a
-- vector into a vector of 1D systolic arrays. 
--
-- The following example assumes fully applied processes that take
-- exactly two signals as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-cascade-formula.png>> <<includes/figs/skel-cascade-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors
-- below. Check the interfaces in @ghci@ to see their usage. To create
-- your own constructors please follow the examples set in the source
-- code.
--
-- > cascade0, cascade1, cascade2, cascade3, cascade4, 
cascade2 :: (a2 -> a1 -> a -> a -> a)  -- ^ @function41@ which needs to be applied to @function21@
         -> Vector (Vector a2)         -- ^ fills in the first argument in the function above
         -> Vector (Vector a1)         -- ^ fills in the second argument in the function above
         -> Vector a                   -- ^ first input vector (e.g. of signals)
         -> Vector a                   -- ^ second input vector (e.g. of signals)
         -> Vector a                   -- ^ output

-- | @mesh@ creates a 2D systolic array, similar to 'cascade2'.
--
-- The following example assumes fully applied processes that take
-- exactly two signals as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-mesh-formula.png>> <<includes/figs/skel-mesh-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors
-- below. Check the interfaces in @ghci@ to see their usage. To create
-- your own constructors please follow the examples set in the source
-- code.
--
-- > mesh0, mesh1, mesh2, mesh3, mesh4, 
mesh2 :: (a2 -> a1 -> a -> a -> a)  -- ^ @function41@ which needs to be applied to @function21@
      -> Vector (Vector a2)         -- ^ fills in the first argument in the function above
      -> Vector (Vector a1)         -- ^ fills in the second argument in the function above
      -> Vector a                   -- ^ first input vector (e.g. of signals)
      -> Vector a                   -- ^ second input vector (e.g. of signals)
      -> Vector (Vector a)          -- ^ output, a 2D vector

--------------- END DOCUMENTATION ---------------