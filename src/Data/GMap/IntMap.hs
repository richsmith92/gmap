{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-warn-unused-imports -Wall #-}

module Data.GMap.IntMap
(-- * IntMap type
 IntMap
) where

import Prelude hiding (foldr,map,filter,lookup)
import Data.GMap

import qualified Data.Monoid as M (Monoid(..))
import qualified Data.Foldable as F (Foldable(..))
import Data.Bits(shiftR,(.&.))
import Data.Typeable
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import qualified Data.List as L
import qualified Data.Maybe as MB
import Control.Monad(foldM)

import GHC.Base hiding (map)
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

-- | Type synonym used to distinguish a key Int# from other Int#.
-- (BTW, the Haddock lies. This synonym is not exported.
-- This is only used in the haddock to distinguish Ints that are Keys from Ints used for other purposes.)
type IntKey = Int#

-- This is basically the same as AVL (from Data.Tree.AVL package) but with an
-- extra Int field (which is unboxed for ghc).
-- | The GT type for 'Int' keys.
data IntMap a = E                                              -- ^ Empty IntMap
             | N {-# UNPACK #-} !IntKey (IntMap a) a (IntMap a)    -- ^ BF=-1 (right height > left height)
             | Z {-# UNPACK #-} !IntKey (IntMap a) a (IntMap a)    -- ^ BF= 0
             | P {-# UNPACK #-} !IntKey (IntMap a) a (IntMap a)    -- ^ BF=+1 (left height > right height)

instance Map IntMap where
 type Key IntMap = Int
-- fromAssocsWith
-- fromAssocsMaybe
 empty                      = emptyIntMap
 nonEmpty                   = nonEmptyIntMap
 status                     = statusIntMap
 addSize                    = addSizeIntMap
 union                      = unionIntMap
 union'                     = unionIntMap'
 unionMaybe                 = unionMaybeIntMap
 disjointUnion              = disjointUnionIntMap
 intersection               = intersectionIntMap
 intersection'              = intersectionIntMap'
 intersectionMaybe          = intersectionMaybeIntMap
 difference                 = differenceIntMap
 differenceMaybe            = differenceMaybeIntMap
 isSubsetOf                 = isSubsetOfIntMap
 isSubmapOf                 = isSubmapOfIntMap
 map                        = mapIntMap
 map'                       = mapIntMap'
 mapMaybe                   = mapMaybeIntMap
 mapWithKey  f imp          = mapWithKeyIntMap  (\i a -> f (I# (i)) a) imp
 mapWithKey' f imp          = mapWithKeyIntMap' (\i a -> f (I# (i)) a) imp
 filter                     = filterIntMap
 foldKeys   f imp b0        = foldKeysAscIntMap     (\i b   -> f (I# (i))   b) imp b0
 foldAssocs   f imp b0      = foldAssocsAscIntMap   (\i a b -> f (I# (i)) a b) imp b0
 foldElems                  = foldElemsAscIntMap
 foldElems'                 = foldElemsAscIntMap'
 foldKeys'    f imp b0      = foldKeysAscIntMap'    (\i b   -> f (I# (i))   b) imp b0
 foldAssocs'  f imp b0      = foldAssocsAscIntMap'  (\i a b -> f (I# (i)) a b) imp b0
 foldElemsUInt              = foldElemsUIntIntMap
 valid                      = validIntMap
 singleton (I# (i)) a            = singletonIntMap i a
 pair (I# (i0)) (I# (i1))        = pairIntMap i0 i1
 lookup       (I# (i)) imp       = lookupIntMap       i imp
 lookupCont f (I# (i)) imp       = lookupContIntMap f i imp
 alter       f (I# (i)) imp      = alterIntMap       f i imp
 insertWith  f (I# (i)) a imp   = insertWithIntMap       f i a imp
 insertWith' f (I# (i)) a imp   = insertWithIntMap'      f i a imp
 insertMaybe  f (I# (i)) a imp   = insertMaybeIntMap  f i a imp
 delete        (I# (i)) imp      = deleteIntMap i imp
 adjustWith   f (I# (i)) imp	 = adjustWithIntMap f i imp
 adjustWith'  f (I# (i)) imp	 = adjustWithIntMap' f i imp
 adjustMaybe f (I# (i)) imp      = adjustMaybeIntMap f i imp
 venn                            = vennIntMap
 venn'                           = vennIntMap'
 vennMaybe                       = vennMaybeIntMap

instance OrderedMap IntMap where
 compareKey                = compareKeyIntMap
 fromAssocsAscWith         = fromAssocsAscWithIntMap
 fromAssocsDescWith        = fromAssocsDescWithIntMap
 fromAssocsAscMaybe        = fromAssocsAscMaybeIntMap
 fromAssocsDescMaybe       = fromAssocsDescMaybeIntMap
 foldKeysAsc     f imp b0 = foldKeysAscIntMap     (\i b   -> f (I# (i))   b) imp b0
 foldKeysDesc    f imp b0 = foldKeysDescIntMap    (\i b   -> f (I# (i))   b) imp b0
 foldAssocsAsc   f imp b0 = foldAssocsAscIntMap   (\i a b -> f (I# (i)) a b) imp b0
 foldAssocsDesc  f imp b0 = foldAssocsDescIntMap  (\i a b -> f (I# (i)) a b) imp b0
 foldElemsAsc        = foldElemsAscIntMap
 foldElemsDesc       = foldElemsDescIntMap
 foldElemsAsc'       = foldElemsAscIntMap'
 foldElemsDesc'      = foldElemsDescIntMap'
 foldKeysAsc'    f imp b0 = foldKeysAscIntMap'    (\i b   -> f (I# (i))   b) imp b0
 foldKeysDesc'   f imp b0 = foldKeysDescIntMap'   (\i b   -> f (I# (i))   b) imp b0
 foldAssocsAsc'  f imp b0 = foldAssocsAscIntMap'  (\i a b -> f (I# (i)) a b) imp b0
 foldAssocsDesc' f imp b0 = foldAssocsDescIntMap' (\i a b -> f (I# (i)) a b) imp b0

-- Local module error prefix
mErr :: String
mErr = "Data.Trie.General.IntMap.Set-"

-- | See 'Map' class method 'empty'.
emptyIntMap :: IntMap a
emptyIntMap = E
{-# INLINE emptyIntMap #-}

-- | See 'Map' class method 'singleton'.
singletonIntMap :: IntKey -> a -> IntMap a
singletonIntMap i a = Z i E a E
{-# INLINE singletonIntMap #-}

-- !!! This might cause problems where the list and the map cant both fit into memory at the same time. Dont use length.
fromAssocsAscIntMap :: [(Int,a)] -> IntMap a
fromAssocsAscIntMap ias = fromAssocsAscLIntMap (length ias) ias
{-# INLINE fromAssocsAscIntMap #-}

fromAssocsDescIntMap :: [(Int,a)] -> IntMap a
fromAssocsDescIntMap ias = fromAssocsDescLIntMap (length ias) ias
{-# INLINE fromAssocsDescIntMap #-}

fromAssocsAscLIntMap :: Int -> [(Int,a)] -> IntMap a
fromAssocsAscLIntMap n ias = case suba (rep n) ias of
                                     (# imp,[] #) -> imp
                                     (# _,_ #)    -> error (mErr ++ "fromAssocsAscLIntMap: List too long.")
 where
 suba  ET      as = (# E,as #)
 suba (NT l r) as = suba_ N l r as
 suba (ZT l r) as = suba_ Z l r as
 suba (PT l r) as = suba_ P l r as
 {-# INLINE suba_ #-}
 suba_ c l r as = case suba l as of
                  (# l_,as_ #) -> case as_ of
                                  (((I# (ka),a):as__)) -> case suba r as__ of
                                                          (# r_,as___ #) -> let t = c ka l_ a r_
                                                                            in t `seq` (# t,as___ #)
                                  [] -> error (mErr ++ "fromAssocsAscLIntMap: List too short.")

fromAssocsDescLIntMap :: Int -> [(Int,a)] -> IntMap a
fromAssocsDescLIntMap n ias = case subd (rep n) ias of
                                      (# imp,[] #) -> imp
                                      (# _,_ #)    -> error (mErr ++ "fromAssocsDescLIntMap: List too long.")
 where
 subd  ET      as = (# E,as #)
 subd (NT l r) as = subd_ N l r as
 subd (ZT l r) as = subd_ Z l r as
 subd (PT l r) as = subd_ P l r as
 {-# INLINE subd_ #-}
 subd_ c l r as = case subd r as of
                  (# r_,as_ #) -> case as_ of
                                  (((I# (ka),a):as__)) -> case subd l as__ of
                                                          (# l_,as___ #) -> let t = c ka l_ a r_
                                                                            in t `seq` (# t,as___ #)
                                  [] -> error (mErr ++ "fromAssocsDescLIntMap: List too short.")

-- Group an ordered list of assocs by key
clump :: Eq k => [(k,a)] -> [(k,[a])]
clump [] = []
clump kas = list' [(k',as' [])]
	where 	(k',as',list') = L.foldl' combine (fst $ head kas,id,id) kas
		-- 'as' and 'list' are list building continuations - so order of 'kas' is preserved
		combine (k1,as,list) (k2,a) =
			if 	k1 == k2
			then	(k1,  as . (a:), list                 )
			else	(k2, (a:),       list . ((k1,as []):) )

fromAssocsAscWithIntMap :: (a -> a -> a) -> [(Int,a)] -> IntMap a
fromAssocsAscWithIntMap f kas = fromAssocsAscIntMap [ (k,L.foldl1' f as) | (k,as) <- clump kas]

fromAssocsDescWithIntMap :: (a -> a -> a) -> [(Int,a)] -> IntMap a
fromAssocsDescWithIntMap f kas = fromAssocsDescIntMap [ (k,L.foldl1' f as) | (k,as) <- clump kas]

fromAssocsAscMaybeIntMap :: (a -> a -> Maybe a) -> [(Int,a)] -> IntMap a
fromAssocsAscMaybeIntMap f kas = fromAssocsAscIntMap $ MB.catMaybes [ fld k as | (k,as) <- clump kas]
	where fld k as = (\a -> (k,a)) `fmap` foldM f (head as) (tail as)
	
fromAssocsDescMaybeIntMap :: (a -> a -> Maybe a) -> [(Int,a)] -> IntMap a
fromAssocsDescMaybeIntMap f kas = fromAssocsDescIntMap $ MB.catMaybes [ fld k as | (k,as) <- clump kas]
	where fld k as = (\a -> (k,a)) `fmap` foldM f (head as) (tail as)

-- | See 'Map' class method 'pair'.
pairIntMap :: IntKey -> IntKey -> Maybe (a -> a -> IntMap a)
pairIntMap i0 i1 = case compareInt# i0 i1 of
                  LT -> Just (\a0 a1 -> P i1 (Z i0 E a0 E) a1 E)
                  EQ -> Nothing
                  GT -> Just (\a0 a1 -> P i0 (Z i1 E a1 E) a0 E)

-- | See 'Map' class method 'nonEmpty'.
nonEmptyIntMap :: IntMap a -> Maybe (IntMap a)
nonEmptyIntMap E   = Nothing
nonEmptyIntMap imp = Just imp

-- | See 'Map' class method 'status'.
statusIntMap :: IntMap a -> Status Int a
statusIntMap E           = None
statusIntMap (Z i E a _) = One (I# (i)) a
statusIntMap _           = Many

{-----------------------------------------
Notes for fast size calculation.
 case (h,avl)
      (0,_      ) -> 0            -- Must be E
      (1,_      ) -> 1            -- Must be (Z  E        _  E       )
      (2,N _ _ _) -> 2            -- Must be (N  E        _ (Z E _ E))
      (2,Z _ _ _) -> 3            -- Must be (Z (Z E _ E) _ (Z E _ E))
      (2,P _ _ _) -> 2            -- Must be (P (Z E _ E) _  E       )
      (3,N _ _ r) -> 2 + size 2 r -- Must be (N (Z E _ E) _  r       )
      (3,P l _ _) -> 2 + size 2 l -- Must be (P  l        _ (Z E _ E))
------------------------------------------}

-- | See 'Map' class method 'addSize'.
addSizeIntMap :: IntMap a -> Int# -> Int#
addSizeIntMap E           n = n
addSizeIntMap (N _ l _ r) n = case addHeight 2# l of
                             2# -> ((n)+#2#)
                             h    -> fasN n h l r
addSizeIntMap (Z _ l _ r) n = case addHeight 1# l of
                             1# -> ((n)+#1#)
                             2# -> ((n)+#3#)
                             h    -> fasZ n h l r
addSizeIntMap (P _ l _ r) n = case addHeight 2# r of
                             2# -> ((n)+#2#)
                             h    -> fasP n h l r

-- Local utilities used by addSizeIntMap, Only work if h >=3 !!
fasN,fasZ,fasP :: Int# -> Int# -> IntMap e -> IntMap e -> Int#
fasN n 3# _ r = fas ((n)+#2#)                    2#       r
fasN n h    l r = fas (fas ((n)+#1#) ((h)-#2#) l) ((h)-#1#) r -- h>=4
fasZ n h    l r = fas (fas ((n)+#1#) ((h)-#1#) l) ((h)-#1#) r
fasP n 3# l _ = fas ((n)+#2#)                    2#       l
fasP n h    l r = fas (fas ((n)+#1#) ((h)-#2#) r) ((h)-#1#) l -- h>=4

-- Local Utility used by fasN,fasZ,fasP, Only works if h >= 2 !!
fas :: Int# -> Int# -> IntMap e -> Int#
fas _ 2#  E          = error "fas: Bug0"
fas n 2# (N _ _ _ _) = ((n)+#2#)
fas n 2# (Z _ _ _ _) = ((n)+#3#)
fas n 2# (P _ _ _ _) = ((n)+#2#)
-- So h must be >= 3 if we get here
fas n h    (N _ l _ r) = fasN n h l r
fas n h    (Z _ l _ r) = fasZ n h l r
fas n h    (P _ l _ r) = fasP n h l r
fas _ _     E          = error "fas: Bug1"
-----------------------------------------------------------------------
------------------------ addSizeIntMap Ends Here -----------------------
-----------------------------------------------------------------------


-- | Adds the height of a tree to the first argument.
--
-- Complexity: O(log n)
addHeight :: Int# -> IntMap e -> Int#
addHeight h  E          = h
addHeight h (N _ l _ _) = addHeight ((h)+#2#) l
addHeight h (Z _ l _ _) = addHeight ((h)+#1#) l
addHeight h (P _ _ _ r) = addHeight ((h)+#2#) r

-- | See 'Map' class method 'lookup'.
lookupIntMap :: IntKey -> IntMap a -> Maybe a
lookupIntMap i0 t = rd t where
 rd  E          = Nothing
 rd (N i l a r) = rd_ i l a r
 rd (Z i l a r) = rd_ i l a r
 rd (P i l a r) = rd_ i l a r
 rd_   i l a r  = case compareInt# i0 i of
                  LT -> rd l
                  EQ -> Just a
                  GT -> rd r

-- | See 'Map' class method 'lookupCont'.
lookupContIntMap :: (a -> Maybe b) -> IntKey -> IntMap a -> Maybe b
lookupContIntMap f i0 t = rd t where
 rd  E          = Nothing
 rd (N i l a r) = rd_ i l a r
 rd (Z i l a r) = rd_ i l a r
 rd (P i l a r) = rd_ i l a r
 rd_   i l a r  = case compareInt# i0 i of
                  LT -> rd l
                  EQ -> f a
                  GT -> rd r

-- | Determine if the supplied key is present in the IntMap.
hasKeyIntMap :: IntMap a -> IntKey -> Bool
hasKeyIntMap t i0 = rd t where
 rd  E          = False
 rd (N i l _ r) = rd_ i l r
 rd (Z i l _ r) = rd_ i l r
 rd (P i l _ r) = rd_ i l r
 rd_   i l   r  = case compareInt# i0 i of
                  LT -> rd l
                  EQ -> True
                  GT -> rd r

-- | Overwrite an existing association pair. This function does not force evaluation of the new associated
-- value. An error is raised if the IntMap does not already contain an entry for the IntKey.
--
-- Complexity: O(log n)
assertWriteIntMap :: IntKey -> a -> IntMap a -> IntMap a
assertWriteIntMap i0 a0 = w where
 w  E          = error "assertWrite: IntKey not found."
 w (N i l a r) = case compareInt# i0 i of
                 LT -> let l' = w l in l' `seq` N i l' a r
                 EQ -> N i0 l a0 r
                 GT -> let r' = w r in r' `seq` N i l  a r'
 w (Z i l a r) = case compareInt# i0 i of
                 LT -> let l' = w l in l' `seq` Z i l' a r
                 EQ -> Z i0 l a0 r
                 GT -> let r' = w r in r' `seq` Z i l  a r'
 w (P i l a r) = case compareInt# i0 i of
                 LT -> let l' = w l in l' `seq` P i l' a r
                 EQ -> P i0 l a0 r
                 GT -> let r' = w r in r' `seq` P i l  a r'

-- | See 'Map' class method 'alter'.
alterIntMap :: (Maybe a -> Maybe a) -> IntKey -> IntMap a -> IntMap a
alterIntMap f i t = case lookupIntMap i t of
                   Nothing -> case f Nothing of
                              Nothing -> t
                              Just a  -> ins i a t
                   ja      -> case f ja of
                              Nothing -> del i t
                              Just a' -> assertWriteIntMap i a' t

-- | See 'Map' class method 'insertMaybe'.
insertMaybeIntMap :: (a -> Maybe a) -> IntKey -> a -> IntMap a -> IntMap a
insertMaybeIntMap f i0 a0 t = case lookupIntMap i0 t of
                             Nothing -> ins i0 a0 t
                             Just a' -> case f a' of
                                        Nothing  -> del i0 t
                                        Just a'' -> assertWriteIntMap i0 a'' t

-- | See 'Map' class method 'delete'.
deleteIntMap :: IntKey -> IntMap a -> IntMap a
deleteIntMap i t = if t `hasKeyIntMap` i then del i t else t

-- | See 'Map' class method 'adjust'.
adjustWithIntMap :: (a -> a) -> IntKey -> IntMap a -> IntMap a
adjustWithIntMap f i t = case lookupIntMap i t of
                         Nothing -> t
                         Just a -> assertWriteIntMap i (f a) t

-- | See 'Map' class method 'adjust''.
adjustWithIntMap' :: (a -> a) -> IntKey -> IntMap a -> IntMap a
adjustWithIntMap' f i t = case lookupIntMap i t of
                         Nothing -> t
                         Just a -> let a' = f a in a' `seq` assertWriteIntMap i a' t

-- | See 'Map' class method 'adjustMaybe'.
adjustMaybeIntMap :: (a -> Maybe a) -> IntKey -> IntMap a -> IntMap a
adjustMaybeIntMap f i t = case lookupIntMap i t of
                         Nothing -> t
                         Just a -> case f a of
                                   Nothing -> del i t
                                   Just a' -> assertWriteIntMap i a' t

-- | See 'Map' class method 'isSubsetOf'.
isSubsetOfIntMap :: IntMap a -> IntMap b -> Bool
isSubsetOfIntMap = s where
 -- s :: IntMap a -> IntMap b -> Bool
 s  E              _             = True
 s  _              E             = False
 s (N ka la _ ra) (N kb lb _ rb) = s' ka la ra kb lb rb
 s (N ka la _ ra) (Z kb lb _ rb) = s' ka la ra kb lb rb
 s (N ka la _ ra) (P kb lb _ rb) = s' ka la ra kb lb rb
 s (Z ka la _ ra) (N kb lb _ rb) = s' ka la ra kb lb rb
 s (Z ka la _ ra) (Z kb lb _ rb) = s' ka la ra kb lb rb
 s (Z ka la _ ra) (P kb lb _ rb) = s' ka la ra kb lb rb
 s (P ka la _ ra) (N kb lb _ rb) = s' ka la ra kb lb rb
 s (P ka la _ ra) (Z kb lb _ rb) = s' ka la ra kb lb rb
 s (P ka la _ ra) (P kb lb _ rb) = s' ka la ra kb lb rb
 s' ka la ra kb lb rb =
  case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT -> case forkL ka lb of
        (# False,_  ,_,_  ,_ #) -> False
        (# True ,llb,_,lrb,_ #) -> (s la llb) && case forkR ra kb of  -- (llb < ka  < kb) & (ka < lrb < kb)
              (# rla,_,rra,_ #) -> (s rla lrb) && (s rra rb)          -- (ka  < rla < kb) & (ka < kb  < rra)
  -- ka = kb
  EQ -> (s la lb) && (s ra rb)
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT -> case forkL ka rb of
        (# False,_  ,_,_  ,_ #) -> False
        (# True ,rlb,_,rrb,_ #) -> (s ra rrb) && case forkR la kb of  -- (kb  < rlb < ka) & (kb < ka  < rrb)
              (# lla,_,lra,_ #) -> (s lra rlb) && (s lla lb)          -- (lla < kb  < ka) & (kb < lra < ka)
 -- forkL returns False if tb does not contain ka (which implies set a cannot be a subset of set b)
 -- forkL :: IntKey -> IntMap b -> (# Bool,IntMap b,Int#,IntMap b,Int# #) -- Vals b..4 only valid if Bool is True!
 forkL ka tb = forkL_ tb 0# where
  forkL_  E          h = (# False,E,h,E,h #)
  forkL_ (N k l b r) h = forkL__ k l ((h)-#2#) b r ((h)-#1#)
  forkL_ (Z k l b r) h = forkL__ k l ((h)-#1#) b r ((h)-#1#)
  forkL_ (P k l b r) h = forkL__ k l ((h)-#1#) b r ((h)-#2#)
  forkL__ k l hl b r hr = case compareInt# ka k of
                          LT -> case forkL_ l hl of
                                (# False,t0,ht0,t1,ht1 #) -> (# False,t0,ht0,t1,ht1 #)
                                (# True ,t0,ht0,t1,ht1 #) -> case spliceH k t1 ht1 b r hr of
                                                             (# t1_,ht1_ #) -> (# True,t0,ht0,t1_,ht1_ #)
                          EQ -> (# True,l,hl,r,hr #)
                          GT -> case forkL_ r hr of
                                (# False,t0,ht0,t1,ht1 #) -> (# False,t0,ht0,t1,ht1 #)
                                (# True ,t0,ht0,t1,ht1 #) -> case spliceH k l hl b t0 ht0 of
                                                             (# t0_,ht0_ #) -> (# True,t0_,ht0_,t1,ht1 #)
 -- forkR discards an element from set a if it is equal to the element from set b
 -- forkR :: IntMap a -> IntKey -> (# IntMap a,Int#,IntMap a,Int# #)
 forkR ta kb = forkR_ ta 0# where
  forkR_  E          h = (# E,h,E,h #) -- Relative heights!!
  forkR_ (N k l a r) h = forkR__ k l ((h)-#2#) a r ((h)-#1#)
  forkR_ (Z k l a r) h = forkR__ k l ((h)-#1#) a r ((h)-#1#)
  forkR_ (P k l a r) h = forkR__ k l ((h)-#1#) a r ((h)-#2#)
  forkR__ k l hl a r hr = case compareInt# k kb of
                          LT -> case forkR_ r hr of
                                (# t0,ht0,t1,ht1 #) -> case spliceH k l hl a t0 ht0 of
                                 (# t0_,ht0_ #)     -> (# t0_,ht0_,t1,ht1 #)
                          EQ -> (# l,hl,r,hr #)     -- e is discarded from set a
                          GT -> case forkR_ l hl of
                                (# t0,ht0,t1,ht1 #) -> case spliceH k t1 ht1 a r hr of
                                 (# t1_,ht1_ #)     -> (# t0,ht0,t1_,ht1_ #)
-----------------------------------------------------------------------
----------------------- isSubsetOfIntMap Ends Here ---------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'isSubmapOf'.
isSubmapOfIntMap :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isSubmapOfIntMap p = s where
 -- s :: IntMap a -> IntMap b -> Bool
 s  E              _             = True
 s  _              E             = False
 s (N ka la a ra) (N kb lb b rb) = s' ka la a ra kb lb b rb
 s (N ka la a ra) (Z kb lb b rb) = s' ka la a ra kb lb b rb
 s (N ka la a ra) (P kb lb b rb) = s' ka la a ra kb lb b rb
 s (Z ka la a ra) (N kb lb b rb) = s' ka la a ra kb lb b rb
 s (Z ka la a ra) (Z kb lb b rb) = s' ka la a ra kb lb b rb
 s (Z ka la a ra) (P kb lb b rb) = s' ka la a ra kb lb b rb
 s (P ka la a ra) (N kb lb b rb) = s' ka la a ra kb lb b rb
 s (P ka la a ra) (Z kb lb b rb) = s' ka la a ra kb lb b rb
 s (P ka la a ra) (P kb lb b rb) = s' ka la a ra kb lb b rb
 s' ka la a ra kb lb b rb =
  case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT -> case forkL ka a lb of
        (# False,_  ,_,_  ,_ #) -> False
        (# True ,llb,_,lrb,_ #) -> (s la llb) && case forkR ra kb b of  -- (llb < ka  < kb) & (ka < lrb < kb)
              (# False,_  ,_,_  ,_ #) -> False
              (# True ,rla,_,rra,_ #) -> (s rla lrb) && (s rra rb)      -- (ka  < rla < kb) & (ka < kb  < rra)
  -- ka = kb
  EQ -> (p a b) && (s la lb) && (s ra rb)
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT -> case forkL ka a rb of
        (# False,_  ,_,_  ,_ #) -> False
        (# True ,rlb,_,rrb,_ #) -> (s ra rrb) && case forkR la kb b of  -- (kb  < rlb < ka) & (kb < ka  < rrb)
              (# False,_  ,_,_  ,_ #) -> False
              (# True, lla,_,lra,_ #) -> (s lra rlb) && (s lla lb)      -- (lla < kb  < ka) & (kb < lra < ka)
 -- forkL returns False if tb does not contain ka (which implies set a cannot be a subset of set b)
 -- forkL :: IntKey -> a -> IntMap b -> (# Bool,IntMap b,Int#,IntMap b,Int# #) -- Vals b..4 only valid if Bool is True!
 forkL ka a tb = forkL_ tb 0# where
  forkL_  E          h = (# False,E,h,E,h #)
  forkL_ (N k l b r) h = forkL__ k l ((h)-#2#) b r ((h)-#1#)
  forkL_ (Z k l b r) h = forkL__ k l ((h)-#1#) b r ((h)-#1#)
  forkL_ (P k l b r) h = forkL__ k l ((h)-#1#) b r ((h)-#2#)
  forkL__ k l hl b r hr = case compareInt# ka k of
                          LT -> case forkL_ l hl of
                                (# False,t0,ht0,t1,ht1 #) -> (# False,t0,ht0,t1,ht1 #)
                                (# True ,t0,ht0,t1,ht1 #) -> case spliceH k t1 ht1 b r hr of
                                                             (# t1_,ht1_ #) -> (# True,t0,ht0,t1_,ht1_ #)
                          EQ -> let bool = p a b in bool `seq` (# bool,l,hl,r,hr #)
                          GT -> case forkL_ r hr of
                                (# False,t0,ht0,t1,ht1 #) -> (# False,t0,ht0,t1,ht1 #)
                                (# True ,t0,ht0,t1,ht1 #) -> case spliceH k l hl b t0 ht0 of
                                                             (# t0_,ht0_ #) -> (# True,t0_,ht0_,t1,ht1 #)
 -- forkR discards an element from set a if it is equal to the element from set b
 -- forkR :: IntMap a -> IntKey -> b -> (# Bool,IntMap a,Int#,IntMap a,Int# #)
 forkR ta kb b = forkR_ ta 0# where
  forkR_  E          h = (# True,E,h,E,h #) -- Relative heights!!
  forkR_ (N k l a r) h = forkR__ k l ((h)-#2#) a r ((h)-#1#)
  forkR_ (Z k l a r) h = forkR__ k l ((h)-#1#) a r ((h)-#1#)
  forkR_ (P k l a r) h = forkR__ k l ((h)-#1#) a r ((h)-#2#)
  forkR__ k l hl a r hr = case compareInt# k kb of
                          LT -> case forkR_ r hr of
                                (# False,t0,ht0,t1,ht1 #) -> (# False,t0,ht0,t1,ht1 #)
                                (# True ,t0,ht0,t1,ht1 #) -> case spliceH k l hl a t0 ht0 of
                                       (# t0_,ht0_ #)     -> (# True,t0_,ht0_,t1,ht1 #)
                          EQ -> let bool = p a b in bool `seq` (# bool,l,hl,r,hr #) -- e is discarded from set a
                          GT -> case forkR_ l hl of
                                (# False,t0,ht0,t1,ht1 #) -> (# False,t0,ht0,t1,ht1 #)
                                (# True ,t0,ht0,t1,ht1 #) -> case spliceH k t1 ht1 a r hr of
                                         (# t1_,ht1_ #)   -> (# True,t0,ht0,t1_,ht1_ #)
-----------------------------------------------------------------------
----------------------- isSubmapOfIntMap Ends Here ---------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'map'.
mapIntMap :: (a -> b) -> IntMap a -> IntMap b
mapIntMap f = mapit where
 mapit  E          = E
 mapit (N i l a r) = let l_ = mapit l
                         r_ = mapit r
                     in l_ `seq` r_ `seq` N i l_ (f a) r_
 mapit (Z i l a r) = let l_ = mapit l
                         r_ = mapit r
                     in l_ `seq` r_ `seq` Z i l_ (f a) r_
 mapit (P i l a r) = let l_ = mapit l
                         r_ = mapit r
                     in l_ `seq` r_ `seq` P i l_ (f a) r_

-- | See 'Map' class method 'map''.
mapIntMap' :: (a -> b) -> IntMap a -> IntMap b
mapIntMap' f = mapit where
 mapit  E          = E
 mapit (N i l a r) = let l_ = mapit l
                         r_ = mapit r
                         b  = f a
                     in b `seq` l_ `seq` r_ `seq` N i l_ b r_
 mapit (Z i l a r) = let l_ = mapit l
                         r_ = mapit r
                         b  = f a
                     in b `seq` l_ `seq` r_ `seq` Z i l_ b r_
 mapit (P i l a r) = let l_ = mapit l
                         r_ = mapit r
                         b  = f a
                     in b `seq` l_ `seq` r_ `seq` P i l_ b r_

-- | See 'Map' class method 'mapMaybe'.
mapMaybeIntMap :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeIntMap f t0 = case mapMaybe_ 0# t0 of (# t_,_ #) -> t_  -- Work with relative heights!!
 where mapMaybe_ h t = case t of
                       E         -> (# E,h #)
                       N i l a r -> m i l ((h)-#2#) a r ((h)-#1#)
                       Z i l a r -> m i l ((h)-#1#) a r ((h)-#1#)
                       P i l a r -> m i l ((h)-#1#) a r ((h)-#2#)
        where m i l hl a r hr =                  case mapMaybe_ hl l of
                                (# l_,hl_ #)  -> case mapMaybe_ hr r of
                                 (# r_,hr_ #) -> case f a of
                                                 Just b  -> spliceH i l_ hl_ b r_ hr_
                                                 Nothing ->   joinH   l_ hl_   r_ hr_

-- | See 'Map' class method 'mapWithKey'.
mapWithKeyIntMap :: (IntKey -> a -> b) -> IntMap a -> IntMap b
mapWithKeyIntMap f = mapit where
 mapit  E          = E
 mapit (N i l a r) = let l_ = mapit l
                         r_ = mapit r
                     in l_ `seq` r_ `seq` N i l_ (f i a) r_
 mapit (Z i l a r) = let l_ = mapit l
                         r_ = mapit r
                     in l_ `seq` r_ `seq` Z i l_ (f i a) r_
 mapit (P i l a r) = let l_ = mapit l
                         r_ = mapit r
                     in l_ `seq` r_ `seq` P i l_ (f i a) r_

-- | See 'Map' class method 'mapWithKey''.
mapWithKeyIntMap' :: (IntKey -> a -> b) -> IntMap a -> IntMap b
mapWithKeyIntMap' f = mapit where
 mapit  E          = E
 mapit (N i l a r) = let l_ = mapit l
                         r_ = mapit r
                         b  = f i a
                     in b `seq` l_ `seq` r_ `seq` N i l_ b r_
 mapit (Z i l a r) = let l_ = mapit l
                         r_ = mapit r
                         b  = f i a
                     in b `seq` l_ `seq` r_ `seq` Z i l_ b r_
 mapit (P i l a r) = let l_ = mapit l
                         r_ = mapit r
                         b  = f i a
                     in b `seq` l_ `seq` r_ `seq` P i l_ b r_

-- | See 'Map' class method 'filter'.
filterIntMap :: (a -> Bool) -> IntMap a -> IntMap a
filterIntMap p t0 = case filter_ 0# t0 of (# _,t_,_ #) -> t_  -- Work with relative heights!!
 where filter_ h t = case t of
                     E         -> (# False,E,h #)
                     N i l e r -> f i l ((h)-#2#) e r ((h)-#1#)
                     Z i l e r -> f i l ((h)-#1#) e r ((h)-#1#)
                     P i l e r -> f i l ((h)-#1#) e r ((h)-#2#)
        where f i l hl e r hr =                     case filter_ hl l of
                                (# bl,l_,hl_ #)  -> case filter_ hr r of
                                 (# br,r_,hr_ #) -> if p e
                                                    then if bl || br
                                                         then case spliceH i l_ hl_ e r_ hr_ of
                                                              (# t_,h_ #) -> (# True,t_,h_ #)
                                                         else (# False,t,h #)
                                                    else case joinH l_ hl_ r_ hr_ of
                                                         (# t_,h_ #) -> (# True,t_,h_ #)

-- | See 'Map' class method 'foldElemsAsc'.
foldElemsAscIntMap :: (a -> b -> b) -> b -> IntMap a -> b
foldElemsAscIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N _ l a r) b = foldV l a r b
 foldU (Z _ l a r) b = foldV l a r b
 foldU (P _ l a r) b = foldV l a r b
 foldV      l a r  b = foldU l (f a (foldU r b))

-- | See 'Map' class method 'foldElemsDesc'.
foldElemsDescIntMap :: (a -> b -> b) -> b -> IntMap a -> b
foldElemsDescIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N _ l a r) b = foldV l a r b
 foldU (Z _ l a r) b = foldV l a r b
 foldU (P _ l a r) b = foldV l a r b
 foldV      l a r  b = foldU r (f a (foldU l b))

-- | See 'Map' class method 'foldKeysAsc'.
foldKeysAscIntMap :: (IntKey -> b -> b) -> b -> IntMap a -> b
foldKeysAscIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l _ r) b = foldV k l r b
 foldU (Z k l _ r) b = foldV k l r b
 foldU (P k l _ r) b = foldV k l r b
 foldV    k l   r  b = foldU l (f k (foldU r b))

-- | See 'Map' class method 'foldKeysDesc'.
foldKeysDescIntMap :: (IntKey -> b -> b) -> b -> IntMap a -> b
foldKeysDescIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l _ r) b = foldV k l r b
 foldU (Z k l _ r) b = foldV k l r b
 foldU (P k l _ r) b = foldV k l r b
 foldV    k l   r  b = foldU r (f k (foldU l b))

-- | See 'Map' class method 'foldAssocsAsc'.
foldAssocsAscIntMap :: (IntKey -> a -> b -> b) -> b -> IntMap a -> b
foldAssocsAscIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l a r) b = foldV k l a r b
 foldU (Z k l a r) b = foldV k l a r b
 foldU (P k l a r) b = foldV k l a r b
 foldV    k l a r  b = foldU l (f k a (foldU r b))

-- | See 'Map' class method 'foldAssocsDesc'.
foldAssocsDescIntMap :: (IntKey -> a -> b -> b) -> b -> IntMap a -> b
foldAssocsDescIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l a r) b = foldV k l a r b
 foldU (Z k l a r) b = foldV k l a r b
 foldU (P k l a r) b = foldV k l a r b
 foldV    k l a r  b = foldU r (f k a (foldU l b))

-- | See 'Map' class method 'foldElemsAsc''.
foldElemsAscIntMap' :: (a -> b -> b) -> b -> IntMap a -> b
foldElemsAscIntMap' f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N _ l a r) b = foldV l a r b
 foldU (Z _ l a r) b = foldV l a r b
 foldU (P _ l a r) b = foldV l a r b
 foldV      l a r  b = let b'  = foldU r b
                           b'' = f a b'
                       in b' `seq` b'' `seq` foldU l b''

-- | See 'Map' class method 'foldElemsDesc''.
foldElemsDescIntMap' :: (a -> b -> b) -> b -> IntMap a -> b
foldElemsDescIntMap' f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N _ l a r) b = foldV l a r b
 foldU (Z _ l a r) b = foldV l a r b
 foldU (P _ l a r) b = foldV l a r b
 foldV      l a r  b = let b'  = foldU l b
                           b'' = f a b'
                       in b' `seq` b'' `seq` foldU r b''

-- | See 'Map' class method 'foldKeysAsc''.
foldKeysAscIntMap' :: (IntKey -> b -> b) -> b -> IntMap a -> b
foldKeysAscIntMap' f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l _ r) b = foldV k l r b
 foldU (Z k l _ r) b = foldV k l r b
 foldU (P k l _ r) b = foldV k l r b
 foldV    k l   r  b = let b'  = foldU r b
                           b'' = f k b'
                       in b' `seq` b'' `seq` foldU l b''

-- | See 'Map' class method 'foldKeysDesc''.
foldKeysDescIntMap' :: (IntKey -> b -> b) -> b -> IntMap a -> b
foldKeysDescIntMap' f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l _ r) b = foldV k l r b
 foldU (Z k l _ r) b = foldV k l r b
 foldU (P k l _ r) b = foldV k l r b
 foldV    k l   r  b = let b'  = foldU l b
                           b'' = f k b'
                       in b' `seq` b'' `seq` foldU r b''

-- | See 'Map' class method 'foldAssocsAsc''.
foldAssocsAscIntMap' :: (IntKey -> a -> b -> b) -> b -> IntMap a -> b
foldAssocsAscIntMap' f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l a r) b = foldV k l a r b
 foldU (Z k l a r) b = foldV k l a r b
 foldU (P k l a r) b = foldV k l a r b
 foldV    k l a r  b = let b'  = foldU r b
                           b'' = f k a b'
                       in b' `seq` b'' `seq` foldU l b''

-- | See 'Map' class method 'foldAssocsDesc''.
foldAssocsDescIntMap' :: (IntKey -> a -> b -> b) -> b -> IntMap a -> b
foldAssocsDescIntMap' f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N k l a r) b = foldV k l a r b
 foldU (Z k l a r) b = foldV k l a r b
 foldU (P k l a r) b = foldV k l a r b
 foldV    k l a r  b = let b'  = foldU l b
                           b'' = f k a b'
                       in b' `seq` b'' `seq` foldU r b''

-- | See 'Map' class method 'foldElemsUInt'.
foldElemsUIntIntMap :: (a -> Int# -> Int#) -> Int# -> IntMap a -> Int#
foldElemsUIntIntMap f bb mp = foldU mp bb  where
 foldU  E          b = b
 foldU (N _ l a r) b = foldV l a r b
 foldU (Z _ l a r) b = foldV l a r b
 foldU (P _ l a r) b = foldV l a r b
 foldV      l a r  b = foldU l (f a (foldU r b))

-- | See 'Map' class method 'valid'.
validIntMap :: IntMap a -> Maybe String
validIntMap imp = if (isBalanced imp) then if (isSorted imp) then Nothing
                                                            else Just "IntMap: Tree is not sorted."
                                     else Just "IntMap: Tree is not balanced."

-- | Verify that an IntMap (tree) is height balanced and that the BF of each node is correct.
--
-- Complexity: O(n)
isBalanced :: IntMap a -> Bool
isBalanced t = not (cH t ==# -1#)

-- Local utility, returns height if balanced, -1 if not
cH :: IntMap a -> Int#
cH  E          = 0#
cH (N _ l _ r) = cH_ 1# l r -- (hr-hl) = 1
cH (Z _ l _ r) = cH_ 0# l r -- (hr-hl) = 0
cH (P _ l _ r) = cH_ 1# r l -- (hl-hr) = 1
cH_ :: Int# -> IntMap a -> IntMap a -> Int#
cH_ delta l r = let hl = cH l
                in if hl ==# -1# then hl
                                   else let hr = cH r
                                        in if hr ==# -1# then hr
                                                           else if ((hr)-#(hl)) ==# delta then ((hr)+#1#)
                                                                                           else -1#

-- | Verify that an IntMap (tree) is sorted.
--
-- Complexity: O(n)
isSorted :: IntMap a -> Bool
isSorted  E          = True
isSorted (N i l _ r) = isSorted_ i l r
isSorted (Z i l _ r) = isSorted_ i l r
isSorted (P i l _ r) = isSorted_ i l r
isSorted_ :: Int# -> IntMap a -> IntMap a -> Bool
isSorted_   i l   r  = (isSortedU l i) && (isSortedL i r)
-- Verify tree is sorted and rightmost element is less than an upper limit (ul)
isSortedU :: IntMap a -> Int# -> Bool
isSortedU  E          _  = True
isSortedU (N i l _ r) ul = isSortedU_ i l r ul
isSortedU (Z i l _ r) ul = isSortedU_ i l r ul
isSortedU (P i l _ r) ul = isSortedU_ i l r ul
isSortedU_ :: Int# -> IntMap a -> IntMap a -> Int# -> Bool
isSortedU_   i l   r  ul = case compareInt# i ul of
                           LT -> (isSortedU l i) && (isSortedLU i r ul)
                           _  -> False
-- Verify tree is sorted and leftmost element is greater than a lower limit (ll)
isSortedL :: Int# -> IntMap a -> Bool
isSortedL  _   E          = True
isSortedL  ll (N i l _ r) = isSortedL_ ll i l r
isSortedL  ll (Z i l _ r) = isSortedL_ ll i l r
isSortedL  ll (P i l _ r) = isSortedL_ ll i l r
isSortedL_ :: Int# -> Int# -> IntMap a -> IntMap a -> Bool
isSortedL_ ll    i l   r  = case compareInt# i ll of
                            GT -> (isSortedLU ll l i) && (isSortedL i r)
                            _  -> False
-- Verify tree is sorted and leftmost element is greater than a lower limit (ll)
-- and rightmost element is less than an upper limit (ul)
isSortedLU :: Int# -> IntMap a -> Int# -> Bool
isSortedLU  _   E          _  = True
isSortedLU  ll (N i l _ r) ul = isSortedLU_ ll i l r ul
isSortedLU  ll (Z i l _ r) ul = isSortedLU_ ll i l r ul
isSortedLU  ll (P i l _ r) ul = isSortedLU_ ll i l r ul
isSortedLU_ :: Int# -> Int# -> IntMap a -> IntMap a -> Int# -> Bool
isSortedLU_ ll    i l   r  ul = case compareInt# i ll of
                                GT -> case compareInt# i ul of
                                      LT -> (isSortedLU ll l i) && (isSortedLU i r ul)
                                      _  -> False
                                _  -> False
-- isSorted ends --
-------------------

-- | See 'Map' class method compareKey
compareKeyIntMap :: IntMap a -> Int -> Int -> Ordering
compareKeyIntMap _ = compare

urk :: String
urk = "Urk .. Bug in IntMap!"

-- | See 'Map' class method 'insert'.
insertWithIntMap :: (a -> a) -> IntKey -> a -> IntMap a -> IntMap a
insertWithIntMap _ k0 a0  E          = Z k0 E a0 E
insertWithIntMap f k0 a0 (N k l a r) = putN f k0 a0 k l a r
insertWithIntMap f k0 a0 (Z k l a r) = putZ f k0 a0 k l a r
insertWithIntMap f k0 a0 (P k l a r) = putP f k0 a0 k l a r

-- | Same as 'insertWithIntMap', but takes the (relative) tree height as an extra argument and
-- returns the updated (relative) tree height.
pushH :: (a -> a) -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a, Int# #)
pushH _ k0 a0 h E           = (# Z k0 E a0 E, ((h)+#1#) #)
pushH f k0 a0 h (N k l a r) = let t_ = putN f k0 a0 k l a r in t_ `seq` (# t_,h #) -- Height can't change
pushH f k0 a0 h (Z k l a r) = let t_ = putZ f k0 a0 k l a r in
                              case t_ of
                              E         -> error urk -- impossible
                              Z _ _ _ _ -> (# t_,        h  #)
                              _         -> (# t_,((h)+#1#) #)
pushH f k0 a0 h (P k l a r) = let t_ = putP f k0 a0 k l a r in t_ `seq` (# t_,h #) -- Height can't change

----------------------------- LEVEL 1 ---------------------------------
--                       putN, putZ, putP                            --
-----------------------------------------------------------------------

-- Put in (N k l a r), BF=-1  , (never returns P)
putN :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putN f k0 a0 k l a r = case compareInt# k0 k of
                       LT -> putNL f k0 a0 k l a r
                       EQ -> let a' = f a in N k0 l a' r
                       GT -> putNR f k0 a0 k l a r

-- Put in (Z k l a r), BF= 0
putZ :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putZ f k0 a0 k l a r = case compareInt# k0 k of
                       LT -> putZL f k0 a0 k l a r
                       EQ -> let a' = f a in Z k0 l a' r
                       GT -> putZR f k0 a0 k l a r

-- Put in (P k l a r), BF=+1 , (never returns N)
putP :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putP f k0 a0 k l a r = case compareInt# k0 k of
                       LT -> putPL f k0 a0 k l a r
                       EQ -> let a' = f a in P k0 l a' r
                       GT -> putPR f k0 a0 k l a r

----------------------------- LEVEL 2 ---------------------------------
--                      putNL, putZL, putPL                          --
--                      putNR, putZR, putPR                          --
-----------------------------------------------------------------------

-- (putNL k l a r): Put in L subtree of (N k l a r), BF=-1 (Never requires rebalancing) , (never returns P)
{-# INLINE putNL #-}
putNL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putNL _ k0 a0 k  E              a r = Z k (Z k0 E a0 E) a r              -- L subtree empty, H:0->1, parent BF:-1-> 0
putNL f k0 a0 k (N lk ll la lr) a r = let l' = putN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                      in l' `seq` N k l' a r
putNL f k0 a0 k (P lk ll la lr) a r = let l' = putP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                      in l' `seq` N k l' a r
putNL f k0 a0 k (Z lk ll la lr) a r = let l' = putZ f k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                      in case l' of
                                      E         -> error urk -- impossible
                                      Z _ _ _ _ -> N k l' a r -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                                      _         -> Z k l' a r -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

-- (putZL k l a r): Put in L subtree of (Z k l a r), BF= 0  (Never requires rebalancing) , (never returns N)
{-# INLINE putZL #-}
putZL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putZL _ k0 a0 k  E              a r = P k (Z k0 E a0 E) a r              -- L subtree        H:0->1, parent BF: 0->+1
putZL f k0 a0 k (N lk ll la lr) a r = let l' = putN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                      in l' `seq` Z k l' a r
putZL f k0 a0 k (P lk ll la lr) a r = let l' = putP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                      in l' `seq` Z k l' a r
putZL f k0 a0 k (Z lk ll la lr) a r = let l' = putZ f k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                      in case l' of
                                      E         -> error urk -- impossible
                                      Z _ _ _ _ -> Z k l' a r -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                      _         -> P k l' a r -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

-- (putZR k l a r): Put in R subtree of (Z k l a r), BF= 0 (Never requires rebalancing) , (never returns P)
{-# INLINE putZR #-}
putZR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putZR _ k0 a0 k l a  E              = N k l a (Z k0 E a0 E)              -- R subtree        H:0->1, parent BF: 0->-1
putZR f k0 a0 k l a (N rk rl ra rr) = let r' = putN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                      in r' `seq` Z k l a r'
putZR f k0 a0 k l a (P rk rl ra rr) = let r' = putP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                      in r' `seq` Z k l a r'
putZR f k0 a0 k l a (Z rk rl ra rr) = let r' = putZ f k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                      in case r' of
                                      E         -> error urk -- impossible
                                      Z _ _ _ _ -> Z k l a r' -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                      _         -> N k l a r' -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

-- (putPR k l a r): Put in R subtree of (P k l a r), BF=+1 (Never requires rebalancing) , (never returns N)
{-# INLINE putPR #-}
putPR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putPR _ k0 a0 k l a  E              = Z k l a (Z k0 E a0 E)              -- R subtree empty, H:0->1,     parent BF:+1-> 0
putPR f k0 a0 k l a (N rk rl ra rr) = let r' = putN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                      in r' `seq` P k l a r'
putPR f k0 a0 k l a (P rk rl ra rr) = let r' = putP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                      in r' `seq` P k l a r'
putPR f k0 a0 k l a (Z rk rl ra rr) = let r' = putZ f k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                      in case r' of
                                      E         -> error urk -- impossible
                                      Z _ _ _ _ -> P k l a r' -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                                      _         -> Z k l a r' -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

     -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

-- (putNR k l a r): Put in R subtree of (N k l a r), BF=-1 , (never returns P)
{-# INLINE putNR #-}
putNR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putNR _ _  _  _ _ _  E              = error urk      -- impossible if BF=-1
putNR f k0 a0 k l a (N rk rl ra rr) = let r' = putN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                      in r' `seq` N k l a r'
putNR f k0 a0 k l a (P rk rl ra rr) = let r' = putP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                      in r' `seq` N k l a r'
putNR f k0 a0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of  -- determine if RR or RL
                                      LT -> putNRL f k0 a0 k l a rk rl ra  rr          -- RL (never returns P)
                                      EQ -> let ra' = f ra in N k l a (Z k0 rl ra' rr) -- new ra
                                      GT -> putNRR f k0 a0 k l a rk rl ra  rr          -- RR (never returns P)

-- (putPL k l a r): Put in L subtree of (P k l a r), BF=+1 , (never returns N)
{-# INLINE putPL #-}
putPL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putPL _ _  _  _  E              _ _ = error urk      -- impossible if BF=+1
putPL f k0 a0 k (N lk ll la lr) a r = let l' = putN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                      in l' `seq` P k l' a r
putPL f k0 a0 k (P lk ll la lr) a r = let l' = putP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                      in l' `seq` P k l' a r
putPL f k0 a0 k (Z lk ll la lr) a r = case compareInt# k0 lk of  -- determine if LL or LR
                                      LT -> putPLL f k0 a0 k lk ll la lr a r           -- LL (never returns N)
                                      EQ -> let la' = f la in P k (Z k0 ll la' lr) a r -- new la
                                      GT -> putPLR f k0 a0 k lk ll la lr a r           -- LR (never returns N)

----------------------------- LEVEL 3 ---------------------------------
--                        putNRR, putPLL                             --
--                        putNRL, putPLR                             --
-----------------------------------------------------------------------

-- (putNRR k l a rk rl ra rr): Put in RR subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE putNRR #-}
putNRR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putNRR _ k0 a0 k l a rk rl ra  E                  = Z rk (Z k l a rl) ra (Z k0 E a0 E)     -- l and rl must also be E, special CASE RR!!
putNRR f k0 a0 k l a rk rl ra (N rrk rrl rra rrr) = let rr' = putN f k0 a0 rrk rrl rra rrr -- RR subtree BF<>0, H:h->h, so no change
                                                    in rr' `seq` N k l a (Z rk rl ra rr')
putNRR f k0 a0 k l a rk rl ra (P rrk rrl rra rrr) = let rr' = putP f k0 a0 rrk rrl rra rrr -- RR subtree BF<>0, H:h->h, so no change
                                                    in rr' `seq` N k l a (Z rk rl ra rr')
putNRR f k0 a0 k l a rk rl ra (Z rrk rrl rra rrr) = let rr' = putZ f k0 a0 rrk rrl rra rrr -- RR subtree BF= 0, so need to look for changes
                                                    in case rr' of
                                                    E         -> error urk -- impossible
                                                    Z _ _ _ _ -> N k l a (Z rk rl ra rr') -- RR subtree BF: 0-> 0, H:h->h, so no change
                                                    _         -> Z rk (Z k l a rl) ra rr' -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

-- (putPLL k lk ll la lr a r): Put in LL subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE putPLL #-}
putPLL :: (a -> a) -> IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
putPLL _ k0 a0 k lk  E                  la lr a r = Z lk (Z k0 E a0 E) la (Z k lr a r)     -- r and lr must also be E, special CASE LL!!
putPLL f k0 a0 k lk (N llk lll lla llr) la lr a r = let ll' = putN f k0 a0 llk lll lla llr -- LL subtree BF<>0, H:h->h, so no change
                                                    in ll' `seq` P k (Z lk ll' la lr) a r
putPLL f k0 a0 k lk (P llk lll lla llr) la lr a r = let ll' = putP f k0 a0 llk lll lla llr -- LL subtree BF<>0, H:h->h, so no change
                                                    in ll' `seq` P k (Z lk ll' la lr) a r
putPLL f k0 a0 k lk (Z llk lll lla llr) la lr a r = let ll' = putZ f k0 a0 llk lll lla llr -- LL subtree BF= 0, so need to look for changes
                                                    in case ll' of
                                                    E         -> error urk -- impossible
                                                    Z _ _ _ _ -> P k (Z lk ll' la lr) a r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                                    _         -> Z lk ll' la (Z k lr a r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

-- (putNRL k l a rk rl ra rr): Put in RL subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE putNRL #-}
putNRL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
putNRL _ k0 a0 k l a rk  E                  ra rr = Z k0 (Z k l a E) a0 (Z rk E ra rr)     -- l and rr must also be E, special CASE LR !!
putNRL f k0 a0 k l a rk (N rlk rll rla rlr) ra rr = let rl' = putN f k0 a0 rlk rll rla rlr -- RL subtree BF<>0, H:h->h, so no change
                                                    in rl' `seq` N k l a (Z rk rl' ra rr)
putNRL f k0 a0 k l a rk (P rlk rll rla rlr) ra rr = let rl' = putP f k0 a0 rlk rll rla rlr -- RL subtree BF<>0, H:h->h, so no change
                                                    in rl' `seq` N k l a (Z rk rl' ra rr)
putNRL f k0 a0 k l a rk (Z rlk rll rla rlr) ra rr = let rl' = putZ f k0 a0 rlk rll rla rlr -- RL subtree BF= 0, so need to look for changes
                                                    in case rl' of
                                                    E                     -> error urk -- impossible
                                                    Z _    _    _    _    -> N k l a (Z rk rl' ra rr)                     -- RL subtree BF: 0-> 0, H:h->h, so no change
                                                    N rlk' rll' rla' rlr' -> Z rlk' (P k l a rll') rla' (Z rk rlr' ra rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                                    P rlk' rll' rla' rlr' -> Z rlk' (Z k l a rll') rla' (N rk rlr' ra rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

-- (putPLR k lk ll la lr a r): Put in LR subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE putPLR #-}
putPLR :: (a -> a) -> IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
putPLR _ k0 a0 k lk ll la  E                  a r = Z k0 (Z lk ll la E) a0 (Z k E a r)      -- r and ll must also be E, special CASE LR !!
putPLR f k0 a0 k lk ll la (N lrk lrl lra lrr) a r = let lr' = putN f k0 a0 lrk lrl lra lrr  -- LR subtree BF<>0, H:h->h, so no change
                                                    in lr' `seq` P k (Z lk ll la lr') a r
putPLR f k0 a0 k lk ll la (P lrk lrl lra lrr) a r = let lr' = putP f k0 a0 lrk lrl lra lrr  -- LR subtree BF<>0, H:h->h, so no change
                                                    in lr' `seq` P k (Z lk ll la lr') a r
putPLR f k0 a0 k lk ll la (Z lrk lrl lra lrr) a r = let lr' = putZ f k0 a0 lrk lrl lra lrr  -- LR subtree BF= 0, so need to look for changes
                                                    in case lr' of
                                                    E                     -> error urk -- impossible
                                                    Z _    _    _    _    -> P k (Z lk ll la lr') a r                     -- LR subtree BF: 0-> 0, H:h->h, so no change
                                                    N lrk' lrl' lra' lrr' -> Z lrk' (P lk ll la lrl') lra' (Z k lrr' a r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                                    P lrk' lrl' lra' lrr' -> Z lrk' (Z lk ll la lrl') lra' (N k lrr' a r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
--------------------- insertWithIntMap/pushH Ends Here ---------------------
-----------------------------------------------------------------------

-----------------------------------------------------------------------
--------------------- insertWithIntMap/pushH Ends Here ---------------------
-----------------------------------------------------------------------

-- | Same as 'insertWithIntMap', but takes the (relative) tree height as an extra argument and
-- returns the updated (relative) tree height.
pushH' -- cpp madness
       :: (a -> a) -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a, Int# #)
pushH' _ k0 a0 h E           = -- cpp madness
                               (# Z k0 E a0 E, ((h)+#1#) #)
pushH' f k0 a0 h (N k l a r) = let t_ = pputN f k0 a0 k l a r in t_ `seq`
                               (# t_,h #) -- Height can't change
pushH' f k0 a0 h (Z k l a r) = let t_ = pputZ f k0 a0 k l a r in
                               case t_ of
                               E         -> error urk -- impossible
                               Z _ _ _ _ -> (# t_,        h  #)
                               _         -> (# t_,((h)+#1#) #)
pushH' f k0 a0 h (P k l a r) = let t_ = pputP f k0 a0 k l a r in t_ `seq`
                               (# t_,h #) -- Height can't change

----------------------------- LEVEL 1 ---------------------------------
--                       pputN, pputZ, pputP                         --
-----------------------------------------------------------------------

-- Put in (N k l a r), BF=-1  , (never returns P)
pputN :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputN f k0 a0 k l a r = case compareInt# k0 k of
                        LT -> pputNL f k0 a0 k l a r
                        EQ -> let a' = f a in a' `seq` N k0 l a' r
                        GT -> pputNR f k0 a0 k l a r

-- Put in (Z k l a r), BF= 0
pputZ :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputZ f k0 a0 k l a r = case compareInt# k0 k of
                        LT -> pputZL f k0 a0 k l a r
                        EQ -> let a' = f a in a' `seq` Z k0 l a' r
                        GT -> pputZR f k0 a0 k l a r

-- Put in (P k l a r), BF=+1 , (never returns N)
pputP :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputP f k0 a0 k l a r = case compareInt# k0 k of
                        LT -> pputPL f k0 a0 k l a r
                        EQ -> let a' = f a in a' `seq` P k0 l a' r
                        GT -> pputPR f k0 a0 k l a r

----------------------------- LEVEL 2 ---------------------------------
--                      pputNL, pputZL, pputPL                       --
--                      pputNR, pputZR, pputPR                       --
-----------------------------------------------------------------------

-- (pputNL k l a r): Put in L subtree of (N k l a r), BF=-1 (Never requires rebalancing) , (never returns P)
{-# INLINE pputNL #-}
pputNL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputNL _ k0 a0 k  E              a r = Z k (Z k0 E a0 E) a r              -- L subtree empty, H:0->1, parent BF:-1-> 0
pputNL f k0 a0 k (N lk ll la lr) a r = let l' = pputN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                       in l' `seq` N k l' a r
pputNL f k0 a0 k (P lk ll la lr) a r = let l' = pputP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                       in l' `seq` N k l' a r
pputNL f k0 a0 k (Z lk ll la lr) a r = let l' = pputZ f k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                       in case l' of
                                       E         -> error urk -- impossible
                                       Z _ _ _ _ -> N k l' a r -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                                       _         -> Z k l' a r -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

-- (pputZL k l a r): Put in L subtree of (Z k l a r), BF= 0  (Never requires rebalancing) , (never returns N)
{-# INLINE pputZL #-}
pputZL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputZL _ k0 a0 k  E              a r = P k (Z k0 E a0 E) a r              -- L subtree        H:0->1, parent BF: 0->+1
pputZL f k0 a0 k (N lk ll la lr) a r = let l' = pputN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                       in l' `seq` Z k l' a r
pputZL f k0 a0 k (P lk ll la lr) a r = let l' = pputP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                       in l' `seq` Z k l' a r
pputZL f k0 a0 k (Z lk ll la lr) a r = let l' = pputZ f k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                       in case l' of
                                       E         -> error urk -- impossible
                                       Z _ _ _ _ -> Z k l' a r -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                       _         -> P k l' a r -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

-- (pputZR k l a r): Put in R subtree of (Z k l a r), BF= 0 (Never requires rebalancing) , (never returns P)
{-# INLINE pputZR #-}
pputZR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputZR _ k0 a0 k l a  E              = N k l a (Z k0 E a0 E)              -- R subtree        H:0->1, parent BF: 0->-1
pputZR f k0 a0 k l a (N rk rl ra rr) = let r' = pputN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                       in r' `seq` Z k l a r'
pputZR f k0 a0 k l a (P rk rl ra rr) = let r' = pputP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                       in r' `seq` Z k l a r'
pputZR f k0 a0 k l a (Z rk rl ra rr) = let r' = pputZ f k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                       in case r' of
                                       E         -> error urk -- impossible
                                       Z _ _ _ _ -> Z k l a r' -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                       _         -> N k l a r' -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

-- (pputPR k l a r): Put in R subtree of (P k l a r), BF=+1 (Never requires rebalancing) , (never returns N)
{-# INLINE pputPR #-}
pputPR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputPR _ k0 a0 k l a  E              = Z k l a (Z k0 E a0 E)              -- R subtree empty, H:0->1,     parent BF:+1-> 0
pputPR f k0 a0 k l a (N rk rl ra rr) = let r' = pputN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                       in r' `seq` P k l a r'
pputPR f k0 a0 k l a (P rk rl ra rr) = let r' = pputP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                       in r' `seq` P k l a r'
pputPR f k0 a0 k l a (Z rk rl ra rr) = let r' = pputZ f k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                       in case r' of
                                       E         -> error urk -- impossible
                                       Z _ _ _ _ -> P k l a r' -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                                       _         -> Z k l a r' -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

     -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

-- (pputNR k l a r): Put in R subtree of (N k l a r), BF=-1 , (never returns P)
{-# INLINE pputNR #-}
pputNR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputNR _ _  _  _ _ _  E              = error urk      -- impossible if BF=-1
pputNR f k0 a0 k l a (N rk rl ra rr) = let r' = pputN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                       in r' `seq` N k l a r'
pputNR f k0 a0 k l a (P rk rl ra rr) = let r' = pputP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                       in r' `seq` N k l a r'
pputNR f k0 a0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of  -- determine if RR or RL
                                       LT -> pputNRL f k0 a0 k l a rk rl ra rr   -- RL (never returns P)
                                       EQ -> let ra' = f ra in ra' `seq` N k l a (Z k0 rl ra' rr)  -- new ra
                                       GT -> pputNRR f k0 a0 k l a rk rl ra rr   -- RR (never returns P)

-- (pputPL k l a r): Put in L subtree of (P k l a r), BF=+1 , (never returns N)
{-# INLINE pputPL #-}
pputPL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputPL _ _  _  _  E              _ _ = error urk      -- impossible if BF=+1
pputPL f k0 a0 k (N lk ll la lr) a r = let l' = pputN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                       in l' `seq` P k l' a r
pputPL f k0 a0 k (P lk ll la lr) a r = let l' = pputP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                       in l' `seq` P k l' a r
pputPL f k0 a0 k (Z lk ll la lr) a r = case compareInt# k0 lk of  -- determine if LL or LR
                                       LT -> pputPLL f k0 a0 k lk ll la lr a r -- LL (never returns N)
                                       EQ -> let la' = f la in la' `seq` P k (Z k0 ll la' lr) a r -- new la
                                       GT -> pputPLR f k0 a0 k lk ll la lr a r -- LR (never returns N)

----------------------------- LEVEL 3 ---------------------------------
--                        pputNRR, pputPLL                           --
--                        pputNRL, pputPLR                           --
-----------------------------------------------------------------------

-- (pputNRR k l a rk rl ra rr): Put in RR subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE pputNRR #-}
pputNRR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputNRR _ k0 a0 k l a rk rl ra  E                  = Z rk (Z k l a rl) ra (Z k0 E a0 E)     -- l and rl must also be E, special CASE RR!!
pputNRR f k0 a0 k l a rk rl ra (N rrk rrl rra rrr) = let rr' = pputN f k0 a0 rrk rrl rra rrr -- RR subtree BF<>0, H:h->h, so no change
                                                     in rr' `seq` N k l a (Z rk rl ra rr')
pputNRR f k0 a0 k l a rk rl ra (P rrk rrl rra rrr) = let rr' = pputP f k0 a0 rrk rrl rra rrr -- RR subtree BF<>0, H:h->h, so no change
                                                     in rr' `seq` N k l a (Z rk rl ra rr')
pputNRR f k0 a0 k l a rk rl ra (Z rrk rrl rra rrr) = let rr' = pputZ f k0 a0 rrk rrl rra rrr -- RR subtree BF= 0, so need to look for changes
                                                     in case rr' of
                                                     E         -> error urk -- impossible
                                                     Z _ _ _ _ -> N k l a (Z rk rl ra rr') -- RR subtree BF: 0-> 0, H:h->h, so no change
                                                     _         -> Z rk (Z k l a rl) ra rr' -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

-- (pputPLL k lk ll la lr a r): Put in LL subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE pputPLL #-}
pputPLL :: (a -> a) -> IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
pputPLL _ k0 a0 k lk  E                  la lr a r = Z lk (Z k0 E a0 E) la (Z k lr a r)     -- r and lr must also be E, special CASE LL!!
pputPLL f k0 a0 k lk (N llk lll lla llr) la lr a r = let ll' = pputN f k0 a0 llk lll lla llr -- LL subtree BF<>0, H:h->h, so no change
                                                     in ll' `seq` P k (Z lk ll' la lr) a r
pputPLL f k0 a0 k lk (P llk lll lla llr) la lr a r = let ll' = pputP f k0 a0 llk lll lla llr -- LL subtree BF<>0, H:h->h, so no change
                                                     in ll' `seq` P k (Z lk ll' la lr) a r
pputPLL f k0 a0 k lk (Z llk lll lla llr) la lr a r = let ll' = pputZ f k0 a0 llk lll lla llr -- LL subtree BF= 0, so need to look for changes
                                                     in case ll' of
                                                     E         -> error urk -- impossible
                                                     Z _ _ _ _ -> P k (Z lk ll' la lr) a r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                                     _         -> Z lk ll' la (Z k lr a r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

-- (pputNRL k l a rk rl ra rr): Put in RL subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE pputNRL #-}
pputNRL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
pputNRL _ k0 a0 k l a rk  E                  ra rr = Z k0 (Z k l a E) a0 (Z rk E ra rr)     -- l and rr must also be E, special CASE LR !!
pputNRL f k0 a0 k l a rk (N rlk rll rla rlr) ra rr = let rl' = pputN f k0 a0 rlk rll rla rlr -- RL subtree BF<>0, H:h->h, so no change
                                                     in rl' `seq` N k l a (Z rk rl' ra rr)
pputNRL f k0 a0 k l a rk (P rlk rll rla rlr) ra rr = let rl' = pputP f k0 a0 rlk rll rla rlr -- RL subtree BF<>0, H:h->h, so no change
                                                     in rl' `seq` N k l a (Z rk rl' ra rr)
pputNRL f k0 a0 k l a rk (Z rlk rll rla rlr) ra rr = let rl' = pputZ f k0 a0 rlk rll rla rlr -- RL subtree BF= 0, so need to look for changes
                                                     in case rl' of
                                                     E                     -> error urk -- impossible
                                                     Z _    _    _    _    -> N k l a (Z rk rl' ra rr)                     -- RL subtree BF: 0-> 0, H:h->h, so no change
                                                     N rlk' rll' rla' rlr' -> Z rlk' (P k l a rll') rla' (Z rk rlr' ra rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                                     P rlk' rll' rla' rlr' -> Z rlk' (Z k l a rll') rla' (N rk rlr' ra rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

-- (pputPLR k lk ll la lr a r): Put in LR subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE pputPLR #-}
pputPLR :: (a -> a) -> IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
pputPLR _ k0 a0 k lk ll la  E                  a r = Z k0 (Z lk ll la E) a0 (Z k E a r)      -- r and ll must also be E, special CASE LR !!
pputPLR f k0 a0 k lk ll la (N lrk lrl lra lrr) a r = let lr' = pputN f k0 a0 lrk lrl lra lrr  -- LR subtree BF<>0, H:h->h, so no change
                                                     in lr' `seq` P k (Z lk ll la lr') a r
pputPLR f k0 a0 k lk ll la (P lrk lrl lra lrr) a r = let lr' = pputP f k0 a0 lrk lrl lra lrr  -- LR subtree BF<>0, H:h->h, so no change
                                                     in lr' `seq` P k (Z lk ll la lr') a r
pputPLR f k0 a0 k lk ll la (Z lrk lrl lra lrr) a r = let lr' = pputZ f k0 a0 lrk lrl lra lrr  -- LR subtree BF= 0, so need to look for changes
                                                     in case lr' of
                                                     E                     -> error urk -- impossible
                                                     Z _    _    _    _    -> P k (Z lk ll la lr') a r                     -- LR subtree BF: 0-> 0, H:h->h, so no change
                                                     N lrk' lrl' lra' lrr' -> Z lrk' (P lk ll la lrl') lra' (Z k lrr' a r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                                     P lrk' lrl' lra' lrr' -> Z lrk' (Z lk ll la lrl') lra' (N k lrr' a r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
-------------------- insertWithIntMap'/pushH' Ends Here --------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'insert'.
insertWithIntMap' -- cpp madness
             :: (a -> a) -> IntKey -> a -> IntMap a -> IntMap a
insertWithIntMap' _ k0 a0  E          = a0 `seq` Z k0 E a0 E
insertWithIntMap' f k0 a0 (N k l a r) = ppputN f k0 a0 k l a r
insertWithIntMap' f k0 a0 (Z k l a r) = ppputZ f k0 a0 k l a r
insertWithIntMap' f k0 a0 (P k l a r) = ppputP f k0 a0 k l a r

{- Not used currently -
-- | Same as 'insertWithIntMap', but takes the (relative) tree height as an extra argument and
-- returns the updated (relative) tree height.
pushH'' -- cpp madness
        :: (a -> a) -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a, Int# #)
pushH'' _ k0 a0 h E           = -- cpp madness
                                a0 `seq` (# Z k0 E a0 E, ((h)+#1#) #)
pushH'' f k0 a0 h (N k l a r) = let t_ = ppputN f k0 a0 k l a r in t_ `seq`
                                (# t_,h #) -- Height can't change
pushH'' f k0 a0 h (Z k l a r) = let t_ = ppputZ f k0 a0 k l a r in
                                case t_ of
                                E         -> error urk -- impossible
                                Z _ _ _ _ -> (# t_,        h  #)
                                _         -> (# t_,((h)+#1#) #)
pushH'' f k0 a0 h (P k l a r) = let t_ = ppputP f k0 a0 k l a r in t_ `seq`
                                (# t_,h #) -- Height can't change
- Not used currently -}

----------------------------- LEVEL 1 ---------------------------------
--                       ppputN, ppputZ, ppputP                      --
-----------------------------------------------------------------------

-- Put in (N k l a r), BF=-1  , (never returns P)
ppputN :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputN f k0 a0 k l a r = case compareInt# k0 k of
                         LT -> ppputNL f k0 a0 k l a r
                         EQ -> let a' = f a in a' `seq` N k0 l a' r
                         GT -> ppputNR f k0 a0 k l a r

-- Put in (Z k l a r), BF= 0
ppputZ :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputZ f k0 a0 k l a r = case compareInt# k0 k of
                         LT -> ppputZL f k0 a0 k l a r
                         EQ -> let a' = f a in a' `seq` Z k0 l a' r
                         GT -> ppputZR f k0 a0 k l a r

-- Put in (P k l a r), BF=+1 , (never returns N)
ppputP :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputP f k0 a0 k l a r = case compareInt# k0 k of
                         LT -> ppputPL f k0 a0 k l a r
                         EQ -> let a' = f a in a' `seq` P k0 l a' r
                         GT -> ppputPR f k0 a0 k l a r

----------------------------- LEVEL 2 ---------------------------------
--                      ppputNL, ppputZL, ppputPL                    --
--                      ppputNR, ppputZR, ppputPR                    --
-----------------------------------------------------------------------

-- (ppputNL k l a r): Put in L subtree of (N k l a r), BF=-1 (Never requires rebalancing) , (never returns P)
{-# INLINE ppputNL #-}
ppputNL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputNL _ k0 a0 k  E              a r = a0 `seq` Z k (Z k0 E a0 E) a r       -- L subtree empty, H:0->1, parent BF:-1-> 0
ppputNL f k0 a0 k (N lk ll la lr) a r = let l' = ppputN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                        in l' `seq` N k l' a r
ppputNL f k0 a0 k (P lk ll la lr) a r = let l' = ppputP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                        in l' `seq` N k l' a r
ppputNL f k0 a0 k (Z lk ll la lr) a r = let l' = ppputZ f k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                        in case l' of
                                        E         -> error urk -- impossible
                                        Z _ _ _ _ -> N k l' a r -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                                        _         -> Z k l' a r -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

-- (ppputZL k l a r): Put in L subtree of (Z k l a r), BF= 0  (Never requires rebalancing) , (never returns N)
{-# INLINE ppputZL #-}
ppputZL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputZL _ k0 a0 k  E              a r = a0 `seq` P k (Z k0 E a0 E) a r       -- L subtree        H:0->1, parent BF: 0->+1
ppputZL f k0 a0 k (N lk ll la lr) a r = let l' = ppputN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                        in l' `seq` Z k l' a r
ppputZL f k0 a0 k (P lk ll la lr) a r = let l' = ppputP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                        in l' `seq` Z k l' a r
ppputZL f k0 a0 k (Z lk ll la lr) a r = let l' = ppputZ f k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                        in case l' of
                                        E         -> error urk -- impossible
                                        Z _ _ _ _ -> Z k l' a r -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                        _         -> P k l' a r -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

-- (ppputZR k l a r): Put in R subtree of (Z k l a r), BF= 0 (Never requires rebalancing) , (never returns P)
{-# INLINE ppputZR #-}
ppputZR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputZR _ k0 a0 k l a  E              = a0 `seq` N k l a (Z k0 E a0 E)       -- R subtree        H:0->1, parent BF: 0->-1
ppputZR f k0 a0 k l a (N rk rl ra rr) = let r' = ppputN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                        in r' `seq` Z k l a r'
ppputZR f k0 a0 k l a (P rk rl ra rr) = let r' = ppputP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                        in r' `seq` Z k l a r'
ppputZR f k0 a0 k l a (Z rk rl ra rr) = let r' = ppputZ f k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                        in case r' of
                                        E         -> error urk -- impossible
                                        Z _ _ _ _ -> Z k l a r' -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                        _         -> N k l a r' -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

-- (ppputPR k l a r): Put in R subtree of (P k l a r), BF=+1 (Never requires rebalancing) , (never returns N)
{-# INLINE ppputPR #-}
ppputPR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputPR _ k0 a0 k l a  E              = a0 `seq` Z k l a (Z k0 E a0 E)       -- R subtree empty, H:0->1,     parent BF:+1-> 0
ppputPR f k0 a0 k l a (N rk rl ra rr) = let r' = ppputN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                        in r' `seq` P k l a r'
ppputPR f k0 a0 k l a (P rk rl ra rr) = let r' = ppputP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                        in r' `seq` P k l a r'
ppputPR f k0 a0 k l a (Z rk rl ra rr) = let r' = ppputZ f k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                        in case r' of
                                        E         -> error urk -- impossible
                                        Z _ _ _ _ -> P k l a r' -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                                        _         -> Z k l a r' -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

     -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

-- (ppputNR k l a r): Put in R subtree of (N k l a r), BF=-1 , (never returns P)
{-# INLINE ppputNR #-}
ppputNR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputNR _ _  _  _ _ _  E              = error urk      -- impossible if BF=-1
ppputNR f k0 a0 k l a (N rk rl ra rr) = let r' = ppputN f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                        in r' `seq` N k l a r'
ppputNR f k0 a0 k l a (P rk rl ra rr) = let r' = ppputP f k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                        in r' `seq` N k l a r'
ppputNR f k0 a0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of  -- determine if RR or RL
                                        LT -> ppputNRL f k0 a0 k l a rk rl ra rr   -- RL (never returns P)
                                        EQ -> let ra' = f ra in ra' `seq` N k l a (Z k0 rl ra' rr)  -- new ra
                                        GT -> ppputNRR f k0 a0 k l a rk rl ra rr   -- RR (never returns P)

-- (ppputPL k l a r): Put in L subtree of (P k l a r), BF=+1 , (never returns N)
{-# INLINE ppputPL #-}
ppputPL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputPL _ _  _  _  E              _ _ = error urk      -- impossible if BF=+1
ppputPL f k0 a0 k (N lk ll la lr) a r = let l' = ppputN f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                        in l' `seq` P k l' a r
ppputPL f k0 a0 k (P lk ll la lr) a r = let l' = ppputP f k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                        in l' `seq` P k l' a r
ppputPL f k0 a0 k (Z lk ll la lr) a r = case compareInt# k0 lk of  -- determine if LL or LR
                                        LT -> ppputPLL f k0 a0 k lk ll la lr a r -- LL (never returns N)
                                        EQ -> let la' = f la in la' `seq` P k (Z k0 ll la' lr) a r -- new la
                                        GT -> ppputPLR f k0 a0 k lk ll la lr a r -- LR (never returns N)

----------------------------- LEVEL 3 ---------------------------------
--                        ppputNRR, ppputPLL                         --
--                        ppputNRL, ppputPLR                         --
-----------------------------------------------------------------------

-- (ppputNRR k l a rk rl ra rr): Put in RR subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE ppputNRR #-}
ppputNRR :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputNRR _ k0 a0 k l a rk rl ra  E                  = a0 `seq` Z rk (Z k l a rl) ra (Z k0 E a0 E) -- l and rl must also be E, special CASE RR!!
ppputNRR f k0 a0 k l a rk rl ra (N rrk rrl rra rrr) = let rr' = ppputN f k0 a0 rrk rrl rra rrr -- RR subtree BF<>0, H:h->h, so no change
                                                      in rr' `seq` N k l a (Z rk rl ra rr')
ppputNRR f k0 a0 k l a rk rl ra (P rrk rrl rra rrr) = let rr' = ppputP f k0 a0 rrk rrl rra rrr -- RR subtree BF<>0, H:h->h, so no change
                                                      in rr' `seq` N k l a (Z rk rl ra rr')
ppputNRR f k0 a0 k l a rk rl ra (Z rrk rrl rra rrr) = let rr' = ppputZ f k0 a0 rrk rrl rra rrr -- RR subtree BF= 0, so need to look for changes
                                                      in case rr' of
                                                      E         -> error urk -- impossible
                                                      Z _ _ _ _ -> N k l a (Z rk rl ra rr') -- RR subtree BF: 0-> 0, H:h->h, so no change
                                                      _         -> Z rk (Z k l a rl) ra rr' -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

-- (ppputPLL k lk ll la lr a r): Put in LL subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE ppputPLL #-}
ppputPLL :: (a -> a) -> IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
ppputPLL _ k0 a0 k lk  E                  la lr a r = a0 `seq` Z lk (Z k0 E a0 E) la (Z k lr a r) -- r and lr must also be E, special CASE LL!!
ppputPLL f k0 a0 k lk (N llk lll lla llr) la lr a r = let ll' = ppputN f k0 a0 llk lll lla llr -- LL subtree BF<>0, H:h->h, so no change
                                                      in ll' `seq` P k (Z lk ll' la lr) a r
ppputPLL f k0 a0 k lk (P llk lll lla llr) la lr a r = let ll' = ppputP f k0 a0 llk lll lla llr -- LL subtree BF<>0, H:h->h, so no change
                                                      in ll' `seq` P k (Z lk ll' la lr) a r
ppputPLL f k0 a0 k lk (Z llk lll lla llr) la lr a r = let ll' = ppputZ f k0 a0 llk lll lla llr -- LL subtree BF= 0, so need to look for changes
                                                      in case ll' of
                                                      E         -> error urk -- impossible
                                                      Z _ _ _ _ -> P k (Z lk ll' la lr) a r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                                      _         -> Z lk ll' la (Z k lr a r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

-- (ppputNRL k l a rk rl ra rr): Put in RL subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE ppputNRL #-}
ppputNRL :: (a -> a) -> IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
ppputNRL _ k0 a0 k l a rk  E                  ra rr = a0 `seq` Z k0 (Z k l a E) a0 (Z rk E ra rr) -- l and rr must also be E, special CASE LR !!
ppputNRL f k0 a0 k l a rk (N rlk rll rla rlr) ra rr = let rl' = ppputN f k0 a0 rlk rll rla rlr -- RL subtree BF<>0, H:h->h, so no change
                                                      in rl' `seq` N k l a (Z rk rl' ra rr)
ppputNRL f k0 a0 k l a rk (P rlk rll rla rlr) ra rr = let rl' = ppputP f k0 a0 rlk rll rla rlr -- RL subtree BF<>0, H:h->h, so no change
                                                      in rl' `seq` N k l a (Z rk rl' ra rr)
ppputNRL f k0 a0 k l a rk (Z rlk rll rla rlr) ra rr = let rl' = ppputZ f k0 a0 rlk rll rla rlr -- RL subtree BF= 0, so need to look for changes
                                                      in case rl' of
                                                      E                     -> error urk -- impossible
                                                      Z _    _    _    _    -> N k l a (Z rk rl' ra rr)                     -- RL subtree BF: 0-> 0, H:h->h, so no change
                                                      N rlk' rll' rla' rlr' -> Z rlk' (P k l a rll') rla' (Z rk rlr' ra rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                                      P rlk' rll' rla' rlr' -> Z rlk' (Z k l a rll') rla' (N rk rlr' ra rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

-- (ppputPLR k lk ll la lr a r): Put in LR subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE ppputPLR #-}
ppputPLR :: (a -> a) -> IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
ppputPLR _ k0 a0 k lk ll la  E                  a r = a0 `seq` Z k0 (Z lk ll la E) a0 (Z k E a r) -- r and ll must also be E, special CASE LR !!
ppputPLR f k0 a0 k lk ll la (N lrk lrl lra lrr) a r = let lr' = ppputN f k0 a0 lrk lrl lra lrr  -- LR subtree BF<>0, H:h->h, so no change
                                                      in lr' `seq` P k (Z lk ll la lr') a r
ppputPLR f k0 a0 k lk ll la (P lrk lrl lra lrr) a r = let lr' = ppputP f k0 a0 lrk lrl lra lrr  -- LR subtree BF<>0, H:h->h, so no change
                                                      in lr' `seq` P k (Z lk ll la lr') a r
ppputPLR f k0 a0 k lk ll la (Z lrk lrl lra lrr) a r = let lr' = ppputZ f k0 a0 lrk lrl lra lrr  -- LR subtree BF= 0, so need to look for changes
                                                      in case lr' of
                                                      E                     -> error urk -- impossible
                                                      Z _    _    _    _    -> P k (Z lk ll la lr') a r                     -- LR subtree BF: 0-> 0, H:h->h, so no change
                                                      N lrk' lrl' lra' lrr' -> Z lrk' (P lk ll la lrl') lra' (Z k lrr' a r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                                      P lrk' lrl' lra' lrr' -> Z lrk' (Z lk ll la lrl') lra' (N k lrr' a r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
------------------ insertWithIntMap'/pushH'' Ends Here --------------------
-----------------------------------------------------------------------

-- | Local insertion facility which just overwrites any existing entry.
ins :: IntKey -> a -> IntMap a -> IntMap a
ins k0 a0  E          = Z k0 E a0 E
ins k0 a0 (N k l a r) = insN k0 a0 k l a r
ins k0 a0 (Z k l a r) = insZ k0 a0 k l a r
ins k0 a0 (P k l a r) = insP k0 a0 k l a r

-- | Same as 'ins', but takes the (relative) tree height as an extra argument and
-- returns the updated (relative) tree height.
insH :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a, Int# #)
insH k0 a0 h E           = (# Z k0 E a0 E, ((h)+#1#) #)
insH k0 a0 h (N k l a r) = let t_ = insN k0 a0 k l a r in t_ `seq` (# t_,h #) -- Height can't change
insH k0 a0 h (Z k l a r) = let t_ = insZ k0 a0 k l a r in
                           case t_ of
                           N _ _ _ _ -> (# t_,((h)+#1#) #)
                           P _ _ _ _ -> (# t_,((h)+#1#) #)
                           _         -> (# t_,        h  #)
insH k0 a0 h (P k l a r) = let t_ = insP k0 a0 k l a r in t_ `seq` (# t_,h #) -- Height can't change

----------------------------- LEVEL 1 ---------------------------------
--                       insN, insZ, insP                            --
-----------------------------------------------------------------------

-- Put in (N k l a r), BF=-1  , (never returns P)
insN :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insN k0 a0 k l a r = case compareInt# k0 k of
                     LT -> insNL k0 a0 k l a r
                     EQ -> N k l a0 r
                     GT -> insNR k0 a0 k l a r

-- Put in (Z k l a r), BF= 0
insZ :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insZ k0 a0 k l a r = case compareInt# k0 k of
                     LT -> insZL k0 a0 k l a r
                     EQ -> Z k l a0 r
                     GT -> insZR k0 a0 k l a r

-- Put in (P k l a r), BF=+1 , (never returns N)
insP :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insP k0 a0 k l a r = case compareInt# k0 k of
                     LT -> insPL k0 a0 k l a r
                     EQ -> P k l a0 r
                     GT -> insPR k0 a0 k l a r

----------------------------- LEVEL 2 ---------------------------------
--                      insNL, insZL, insPL                          --
--                      insNR, insZR, insPR                          --
-----------------------------------------------------------------------

-- (insNL k l a r): Put in L subtree of (N k l a r), BF=-1 (Never requires rebalancing) , (never returns P)
{-# INLINE insNL #-}
insNL :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insNL k0 a0 k  E              a r = Z k (Z k0 E a0 E) a r            -- L subtree empty, H:0->1, parent BF:-1-> 0
insNL k0 a0 k (N lk ll la lr) a r = let l' = insN k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                    in l' `seq` N k l' a r
insNL k0 a0 k (P lk ll la lr) a r = let l' = insP k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                                    in l' `seq` N k l' a r
insNL k0 a0 k (Z lk ll la lr) a r = let l' = insZ k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                    in case l' of
                                    E         -> error urk -- impossible
                                    Z _ _ _ _ -> N k l' a r -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                                    _         -> Z k l' a r -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

-- (insZL k l a r): Put in L subtree of (Z k l a r), BF= 0  (Never requires rebalancing) , (never returns N)
{-# INLINE insZL #-}
insZL :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insZL k0 a0 k  E              a r = P k (Z k0 E a0 E) a r            -- L subtree        H:0->1, parent BF: 0->+1
insZL k0 a0 k (N lk ll la lr) a r = let l' = insN k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                    in l' `seq` Z k l' a r
insZL k0 a0 k (P lk ll la lr) a r = let l' = insP k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                                    in l' `seq` Z k l' a r
insZL k0 a0 k (Z lk ll la lr) a r = let l' = insZ k0 a0 lk ll la lr  -- L subtree BF= 0, so need to look for changes
                                    in case l' of
                                    E         -> error urk -- impossible
                                    Z _ _ _ _ -> Z k l' a r -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                    _         -> P k l' a r -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

-- (insZR k l a r): Put in R subtree of (Z k l a r), BF= 0 (Never requires rebalancing) , (never returns P)
{-# INLINE insZR #-}
insZR :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insZR k0 a0 k l a  E              = N k l a (Z k0 E a0 E)            -- R subtree        H:0->1, parent BF: 0->-1
insZR k0 a0 k l a (N rk rl ra rr) = let r' = insN k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                    in r' `seq` Z k l a r'
insZR k0 a0 k l a (P rk rl ra rr) = let r' = insP k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                                    in r' `seq` Z k l a r'
insZR k0 a0 k l a (Z rk rl ra rr) = let r' = insZ k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                    in case r' of
                                    E         -> error urk -- impossible
                                    Z _ _ _ _ -> Z k l a r' -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                                    _         -> N k l a r' -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

-- (insPR k l a r): Put in R subtree of (P k l a r), BF=+1 (Never requires rebalancing) , (never returns N)
{-# INLINE insPR #-}
insPR :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insPR k0 a0 k l a  E              = Z k l a (Z k0 E a0 E)            -- R subtree empty, H:0->1,     parent BF:+1-> 0
insPR k0 a0 k l a (N rk rl ra rr) = let r' = insN k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                    in r' `seq` P k l a r'
insPR k0 a0 k l a (P rk rl ra rr) = let r' = insP k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                                    in r' `seq` P k l a r'
insPR k0 a0 k l a (Z rk rl ra rr) = let r' = insZ k0 a0 rk rl ra rr  -- R subtree BF= 0, so need to look for changes
                                    in case r' of
                                    E         -> error urk -- impossible
                                    Z _ _ _ _ -> P k l a r' -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                                    _         -> Z k l a r' -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

     -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

-- (insNR k l a r): Put in R subtree of (N k l a r), BF=-1 , (never returns P)
{-# INLINE insNR #-}
insNR :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insNR _  _  _ _ _  E              = error urk            -- impossible if BF=-1
insNR k0 a0 k l a (N rk rl ra rr) = let r' = insN k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                    in r' `seq` N k l a r'
insNR k0 a0 k l a (P rk rl ra rr) = let r' = insP k0 a0 rk rl ra rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                                    in r' `seq` N k l a r'
insNR k0 a0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of  -- determine if RR or RL
                                    LT -> insNRL k0 a0 k l a rk rl ra  rr   -- RL (never returns P)
                                    EQ -> N k l a (Z rk rl a0 rr)
                                    GT -> insNRR k0 a0 k l a rk rl ra  rr   -- RR (never returns P)

-- (insPL k l a r): Put in L subtree of (P k l a r), BF=+1 , (never returns N)
{-# INLINE insPL #-}
insPL :: IntKey -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insPL _  _  _  E              _ _ = error urk            -- impossible if BF=+1
insPL k0 a0 k (N lk ll la lr) a r = let l' = insN k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                    in l' `seq` P k l' a r
insPL k0 a0 k (P lk ll la lr) a r = let l' = insP k0 a0 lk ll la lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                                    in l' `seq` P k l' a r
insPL k0 a0 k (Z lk ll la lr) a r = case compareInt# k0 lk of        -- determine if LL or LR
                                    LT -> insPLL k0 a0 k lk ll la  lr  a r -- LL (never returns N)
                                    EQ -> P k (Z lk ll a0 lr) a r
                                    GT -> insPLR k0 a0 k lk ll la  lr  a r -- LR (never returns N)

----------------------------- LEVEL 3 ---------------------------------
--                        insNRR, insPLL                             --
--                        insNRL, insPLR                             --
-----------------------------------------------------------------------

-- (insNRR k l a rk rl ra rr): Put in RR subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE insNRR #-}
insNRR :: IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insNRR k0 a0 k l a rk rl ra  E                  = Z rk (Z k l a rl) ra (Z k0 E a0 E)    -- l and rl must also be E, special CASE RR!!
insNRR k0 a0 k l a rk rl ra (N rrk rrl rra rrr) = let rr' = insN k0 a0 rrk rrl rra rrr  -- RR subtree BF<>0, H:h->h, so no change
                                                  in rr' `seq` N k l a (Z rk rl ra rr')
insNRR k0 a0 k l a rk rl ra (P rrk rrl rra rrr) = let rr' = insP k0 a0 rrk rrl rra rrr  -- RR subtree BF<>0, H:h->h, so no change
                                                  in rr' `seq` N k l a (Z rk rl ra rr')
insNRR k0 a0 k l a rk rl ra (Z rrk rrl rra rrr) = let rr' = insZ k0 a0 rrk rrl rra rrr  -- RR subtree BF= 0, so need to look for changes
                                                  in case rr' of
                                                  E         -> error urk    -- impossible
                                                  Z _ _ _ _ -> N k l a (Z rk rl ra rr') -- RR subtree BF: 0-> 0, H:h->h, so no change
                                                  _         -> Z rk (Z k l a rl) ra rr' -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

-- (insPLL k lk ll la lr a r): Put in LL subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE insPLL #-}
insPLL :: IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
insPLL k0 a0 k lk  E                  la lr a r = Z lk (Z k0 E a0 E) la (Z k lr a r)    -- r and lr must also be E, special CASE LL!!
insPLL k0 a0 k lk (N llk lll lla llr) la lr a r = let ll' = insN k0 a0 llk lll lla llr  -- LL subtree BF<>0, H:h->h, so no change
                                                  in ll' `seq` P k (Z lk ll' la lr) a r
insPLL k0 a0 k lk (P llk lll lla llr) la lr a r = let ll' = insP k0 a0 llk lll lla llr  -- LL subtree BF<>0, H:h->h, so no change
                                                  in ll' `seq` P k (Z lk ll' la lr) a r
insPLL k0 a0 k lk (Z llk lll lla llr) la lr a r = let ll' = insZ k0 a0 llk lll lla llr  -- LL subtree BF= 0, so need to look for changes
                                                  in case ll' of
                                                  E         -> error urk    -- impossible
                                                  Z _ _ _ _ -> P k (Z lk ll' la lr) a r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                                  _         -> Z lk ll' la (Z k lr a r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

-- (insNRL k l a rk rl ra rr): Put in RL subtree of (N k l a (Z rk rl ra rr)) , (never returns P)
{-# INLINE insNRL #-}
insNRL :: IntKey -> a -> IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
insNRL k0 a0 k l a rk  E                  ra rr = Z k0 (Z k l a E) a0 (Z rk E ra rr)    -- l and rr must also be E, special CASE LR !!
insNRL k0 a0 k l a rk (N rlk rll rla rlr) ra rr = let rl' = insN k0 a0 rlk rll rla rlr  -- RL subtree BF<>0, H:h->h, so no change
                                                  in rl' `seq` N k l a (Z rk rl' ra rr)
insNRL k0 a0 k l a rk (P rlk rll rla rlr) ra rr = let rl' = insP k0 a0 rlk rll rla rlr  -- RL subtree BF<>0, H:h->h, so no change
                                                  in rl' `seq` N k l a (Z rk rl' ra rr)
insNRL k0 a0 k l a rk (Z rlk rll rla rlr) ra rr = let rl' = insZ k0 a0 rlk rll rla rlr  -- RL subtree BF= 0, so need to look for changes
                                                  in case rl' of
                                                  E                     -> error urk -- impossible
                                                  Z _    _    _    _    -> N k l a (Z rk rl' ra rr)                     -- RL subtree BF: 0-> 0, H:h->h, so no change
                                                  N rlk' rll' rla' rlr' -> Z rlk' (P k l a rll') rla' (Z rk rlr' ra rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                                  P rlk' rll' rla' rlr' -> Z rlk' (Z k l a rll') rla' (N rk rlr' ra rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

-- (insPLR k lk ll la lr a r): Put in LR subtree of (P k (Z lk ll la lr) a r) , (never returns N)
{-# INLINE insPLR #-}
insPLR :: IntKey -> a -> IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> IntMap a
insPLR k0 a0 k lk ll la  E                  a r = Z k0 (Z lk ll la E) a0 (Z k E a r)     -- r and ll must also be E, special CASE LR !!
insPLR k0 a0 k lk ll la (N lrk lrl lra lrr) a r = let lr' = insN k0 a0 lrk lrl lra lrr   -- LR subtree BF<>0, H:h->h, so no change
                                                  in lr' `seq` P k (Z lk ll la lr') a r
insPLR k0 a0 k lk ll la (P lrk lrl lra lrr) a r = let lr' = insP k0 a0 lrk lrl lra lrr   -- LR subtree BF<>0, H:h->h, so no change
                                                  in lr' `seq` P k (Z lk ll la lr') a r
insPLR k0 a0 k lk ll la (Z lrk lrl lra lrr) a r = let lr' = insZ k0 a0 lrk lrl lra lrr   -- LR subtree BF= 0, so need to look for changes
                                                  in case lr' of
                                                  E                     -> error urk -- impossible
                                                  Z _    _    _    _    -> P k (Z lk ll la lr') a r                     -- LR subtree BF: 0-> 0, H:h->h, so no change
                                                  N lrk' lrl' lra' lrr' -> Z lrk' (P lk ll la lrl') lra' (Z k lrr' a r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                                  P lrk' lrl' lra' lrr' -> Z lrk' (Z lk ll la lrl') lra' (N k lrr' a r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
-------------------------- ins/insH End Here --------------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'union'.
unionIntMap :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionIntMap f t0_ t1_ = u0 t0_ t1_ where
 u0     E            t1               = t1
 u0 t0                   E            = t0
 u0 t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = uH (addHeight 2# l0) t0 (addHeight 2# l1) t1
 u0 t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = uH (addHeight 2# l0) t0 (addHeight 1# l1) t1
 u0 t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = uH (addHeight 2# l0) t0 (addHeight 2# r1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = uH (addHeight 1# l0) t0 (addHeight 2# l1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = uH (addHeight 1# l0) t0 (addHeight 1# l1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = uH (addHeight 1# l0) t0 (addHeight 2# r1) t1
 u0 t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = uH (addHeight 2# r0) t0 (addHeight 2# l1) t1
 u0 t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = uH (addHeight 2# r0) t0 (addHeight 1# l1) t1
 u0 t0@(P _ _  _ r0) t1@(P _ _  _ r1) = uH (addHeight 2# r0) t0 (addHeight 2# r1) t1
 -- uH :: Int# -> IntMap a ->   -- 1st IntMap with height
 --       Int# -> IntMap a ->   -- 2nd IntMap with height
 --       IntMap a
 uH h0 t0 h1 t1 = case u h0 t0 h1 t1 of (# t,_ #) -> t
 -- u :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --      Int# -> IntMap a  ->    -- 2nd IntMap with height
 --      (# Int#,IntMap a #)     -- Output IntMap with height
 ------------------------------------------------
 u 0# _    h1              t1              = (# t1,h1 #)
 u h0   t0   0#            _               = (# t0,h0 #)
 ------------------------------------------------
 u 1# (Z k0 _  a0 _ ) 1# t1@(Z k1 _  a1 _ ) = case compareInt# k0 k1 of
                                                  LT -> (# N k0  E  a0        t1, 2# #)
                                                  EQ -> (# Z k0  E  (f a0 a1) E , 1# #)
                                                  GT -> (# P k0  t1 a0        E , 2# #)
 u 1# (Z k0 _  a0 _ ) ht1  t1              = pushAB k0 a0 ht1 t1
 u ht0  t0              1# (Z k1 _  a1 _ ) = pushBA k1 a1 ht0 t0
 ------------------------------------------------
 u 2# (N k0 _ a0 (Z k0_ _ a0_ _)) ht1 t1 = pushAB2 k0 a0 k0_ a0_ ht1 t1
 u 2# (P k0_ (Z k0 _ a0 _) a0_ _) ht1 t1 = pushAB2 k0 a0 k0_ a0_ ht1 t1
 u ht0 t0 2# (N k1 _ a1 (Z k1_ _ a1_ _)) = pushBA2 k1 a1 k1_ a1_ ht0 t0
 u ht0 t0 2# (P k1_ (Z k1 _ a1 _) a1_ _) = pushBA2 k1 a1 k1_ a1_ ht0 t0
 u 2# (Z k0_ (Z k0 _ a0 _) a0_ (Z k0__ _ a0__ _)) ht1 t1 = pushAB3 k0 a0 k0_ a0_ k0__ a0__ ht1 t1
 u ht0 t0 2# (Z k1_ (Z k1 _ a1 _) a1_ (Z k1__ _ a1__ _)) = pushBA3 k1 a1 k1_ a1_ k1__ a1__ ht0 t0
 ------------------------------------------------
 u h0 (N k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (N k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (N k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u h0 (Z k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (Z k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (Z k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u h0 (P k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (P k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (P k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u _  _               _  _               = error (mErr ++ "unionIntMap: Bad IntMap.")
 u_ k0 hl0 l0 a0 hr0 r0 k1 hl1 l1 a1 hr1 r1 =
  case compareInt# k0 k1 of
  -- k0 < k1, so (l0 < k0 < k1) & (k0 < k1 < r1)
  LT ->                                 case forkR hr0 r0 k1 a1 of
        (# hrl0,rl0,a1_,hrr0,rr0 #)  -> case forkL k0 a0 hl1 l1 of -- (k0  < rl0 < k1) & (k0 < k1  < rr0)
         (# hll1,ll1,a0_,hlr1,lr1 #) ->                            -- (ll1 < k0  < k1) & (k0 < lr1 < k1)
          -- (l0 + ll1) < k0 < (rl0 + lr1) < k1 < (rr0 + r1)
                                        case u  hl0  l0 hll1 ll1 of
          (# l,hl #)                 -> case u hrl0 rl0 hlr1 lr1 of
           (# m,hm #)                -> case u hrr0 rr0  hr1  r1 of
            (# r,hr #)               -> case spliceH k1 m hm a1_ r hr of
             (# t,ht #)              -> spliceH k0 l hl a0_ t ht
  -- k0 = k1
  EQ ->                case u hl0 l0 hl1 l1 of
        (# l,hl #)  -> case u hr0 r0 hr1 r1 of
         (# r,hr #) -> spliceH k0 l hl (f a0 a1) r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                                 case forkL k0 a0 hr1 r1 of
        (# hrl1,rl1,a0_,hrr1,rr1 #)  -> case forkR hl0 l0 k1 a1 of -- (k1  < rl1 < k0) & (k1 < k0  < rr1)
         (# hll0,ll0,a1_,hlr0,lr0 #) ->                            -- (ll0 < k1  < k0) & (k1 < lr0 < k0)
          -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                        case u hll0 ll0  hl1  l1 of
          (# l,hl #)                 -> case u hlr0 lr0 hrl1 rl1 of
           (# m,hm #)                -> case u  hr0  r0 hrr1 rr1 of
            (# r,hr #)               -> case spliceH k1 l hl a1_ m hm of
             (# t,ht #)              -> spliceH k0 t ht a0_ r hr
 -- We need 2 different versions of fork (L & R) to ensure that values are combined in
 -- the right order (f a0 a1)
 ------------------------------------------------
 -- forkL :: IntKey -> a -> Int# -> IntMap a -> (# Int#,IntMap a,a,Int#,IntMap a #)
 forkL k0 a0 ht1 t1 = forkL_ ht1 t1 where
  forkL_ h  E          = (# h,E,a0,h,E #)
  forkL_ h (N k l a r) = forkL__ k ((h)-#2#) l a ((h)-#1#) r
  forkL_ h (Z k l a r) = forkL__ k ((h)-#1#) l a ((h)-#1#) r
  forkL_ h (P k l a r) = forkL__ k ((h)-#1#) l a ((h)-#2#) r
  forkL__ k hl l a hr r = case compareInt# k0 k of
                          LT ->                            case forkL_ hl l of
                                (# hl0,l0,a0_,hl1,l1 #) -> case spliceH k l1 hl1 a r hr of
                                 (# l1_,hl1_ #)         -> (# hl0,l0,a0_,hl1_,l1_ #)
                          EQ ->                            (# hl,l,f a0 a,hr,r #)
                          GT ->                            case forkL_ hr r of
                                (# hl0,l0,a0_,hl1,l1 #) -> case spliceH k l hl a l0 hl0 of
                                 (# l0_,hl0_ #)         -> (# hl0_,l0_,a0_,hl1,l1 #)
 ------------------------------------------------
 -- forkL :: Int# -> IntMap a -> IntKey -> a -> (# Int#,IntMap a,a,Int#,IntMap a #)
 forkR ht0 t0 k1 a1 = forkR_ ht0 t0 where
  forkR_ h  E          = (# h,E,a1,h,E #)
  forkR_ h (N k l a r) = forkR__ k ((h)-#2#) l a ((h)-#1#) r
  forkR_ h (Z k l a r) = forkR__ k ((h)-#1#) l a ((h)-#1#) r
  forkR_ h (P k l a r) = forkR__ k ((h)-#1#) l a ((h)-#2#) r
  forkR__ k hl l a hr r = case compareInt# k k1 of
                          LT ->                            case forkR_ hr r of
                                (# hl0,l0,a1_,hl1,l1 #) -> case spliceH k l hl a l0 hl0 of
                                 (# l0_,hl0_ #)         -> (# hl0_,l0_,a1_,hl1,l1 #)
                          EQ ->                            (# hl,l,f a a1,hr,r #)
                          GT ->                            case forkR_ hl l of
                                (# hl0,l0,a1_,hl1,l1 #) -> case spliceH k l1 hl1 a r hr of
                                 (# l1_,hl1_ #)         -> (# hl0,l0,a1_,hl1_,l1_ #)
 ------------------------------------------------
 -- pushAB :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB k0 a0 ht1 t1 = pushH (\a1 -> f a0 a1) k0 a0 ht1 t1
 ------------------------------------------------
 -- pushBA :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA k1 a1 ht0 t0 = pushH (\a0 -> f a0 a1) k1 a1 ht0 t0
 ------------------------------------------------
 -- pushAB2 :: IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB2 k0 a0 k0_ a0_ ht1 t1 = case pushAB k0_ a0_ ht1 t1 of
                                (# t,h #) -> pushAB k0 a0 h t
 ------------------------------------------------
 -- pushBA2 :: IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA2 k1 a1 k1_ a1_ ht0 t0 = case pushBA k1_ a1_ ht0 t0 of
                                (# t,h #) -> pushBA k1 a1 h t
 ------------------------------------------------
 -- pushAB3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB3 k0 a0 k0_ a0_ k0__ a0__ ht1 t1 = case pushAB k0__ a0__ ht1 t1 of
                                          (# t,h #) -> pushAB2 k0 a0 k0_ a0_ h t
 ------------------------------------------------
 -- pushBA3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA3 k1 a1 k1_ a1_ k1__ a1__ ht0 t0 = case pushBA k1__ a1__ ht0 t0 of
                                          (# t,h #) -> pushBA2 k1 a1 k1_ a1_ h t
-----------------------------------------------------------------------
----------------------- unionIntMap Ends Here --------------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'union''.
unionIntMap' :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionIntMap' f t0_ t1_ = u0 t0_ t1_ where
 u0     E            t1               = t1
 u0 t0                   E            = t0
 u0 t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = uH (addHeight 2# l0) t0 (addHeight 2# l1) t1
 u0 t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = uH (addHeight 2# l0) t0 (addHeight 1# l1) t1
 u0 t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = uH (addHeight 2# l0) t0 (addHeight 2# r1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = uH (addHeight 1# l0) t0 (addHeight 2# l1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = uH (addHeight 1# l0) t0 (addHeight 1# l1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = uH (addHeight 1# l0) t0 (addHeight 2# r1) t1
 u0 t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = uH (addHeight 2# r0) t0 (addHeight 2# l1) t1
 u0 t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = uH (addHeight 2# r0) t0 (addHeight 1# l1) t1
 u0 t0@(P _ _  _ r0) t1@(P _ _  _ r1) = uH (addHeight 2# r0) t0 (addHeight 2# r1) t1
 -- uH :: Int# -> IntMap a ->   -- 1st IntMap with height
 --       Int# -> IntMap a ->   -- 2nd IntMap with height
 --       IntMap a
 uH h0 t0 h1 t1 = case u h0 t0 h1 t1 of (# t,_ #) -> t
 -- u :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --      Int# -> IntMap a  ->    -- 2nd IntMap with height
 --      (# Int#,IntMap a #)     -- Output IntMap with height
 ------------------------------------------------
 u 0# _    h1              t1              = (# t1,h1 #)
 u h0   t0   0#            _               = (# t0,h0 #)
 ------------------------------------------------
 u 1# (Z k0 _  a0 _ ) 1# t1@(Z k1 _  a1 _ ) = case compareInt# k0 k1 of
                                                  LT -> (# N k0 E  a0 t1, 2# #)
                                                  EQ -> let a_ = f a0 a1 in a_ `seq`
                                                        (# Z k0 E a_ E , 1# #)
                                                  GT -> (# P k0 t1 a0 E , 2# #)
 u 1# (Z k0 _  a0 _ ) ht1  t1              = pushAB k0 a0 ht1 t1
 u ht0  t0              1# (Z k1 _  a1 _ ) = pushBA k1 a1 ht0 t0
 ------------------------------------------------
 u 2# (N k0 _ a0 (Z k0_ _ a0_ _)) ht1 t1 = pushAB2 k0 a0 k0_ a0_ ht1 t1
 u 2# (P k0_ (Z k0 _ a0 _) a0_ _) ht1 t1 = pushAB2 k0 a0 k0_ a0_ ht1 t1
 u ht0 t0 2# (N k1 _ a1 (Z k1_ _ a1_ _)) = pushBA2 k1 a1 k1_ a1_ ht0 t0
 u ht0 t0 2# (P k1_ (Z k1 _ a1 _) a1_ _) = pushBA2 k1 a1 k1_ a1_ ht0 t0
 u 2# (Z k0_ (Z k0 _ a0 _) a0_ (Z k0__ _ a0__ _)) ht1 t1 = pushAB3 k0 a0 k0_ a0_ k0__ a0__ ht1 t1
 u ht0 t0 2# (Z k1_ (Z k1 _ a1 _) a1_ (Z k1__ _ a1__ _)) = pushBA3 k1 a1 k1_ a1_ k1__ a1__ ht0 t0
 ------------------------------------------------
 u h0 (N k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (N k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (N k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u h0 (Z k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (Z k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (Z k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u h0 (P k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (P k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (P k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u _  _               _  _               = error (mErr ++ "unionIntMap: Bad IntMap.")
 u_ k0 hl0 l0 a0 hr0 r0 k1 hl1 l1 a1 hr1 r1 =
  case compareInt# k0 k1 of
  -- k0 < k1, so (l0 < k0 < k1) & (k0 < k1 < r1)
  LT ->                                 case forkR hr0 r0 k1 a1 of
        (# hrl0,rl0,a1_,hrr0,rr0 #)  -> case forkL k0 a0 hl1 l1 of -- (k0  < rl0 < k1) & (k0 < k1  < rr0)
         (# hll1,ll1,a0_,hlr1,lr1 #) ->                            -- (ll1 < k0  < k1) & (k0 < lr1 < k1)
          -- (l0 + ll1) < k0 < (rl0 + lr1) < k1 < (rr0 + r1)
                                        case u  hl0  l0 hll1 ll1 of
          (# l,hl #)                 -> case u hrl0 rl0 hlr1 lr1 of
           (# m,hm #)                -> case u hrr0 rr0  hr1  r1 of
            (# r,hr #)               -> case spliceH k1 m hm a1_ r hr of
             (# t,ht #)              -> spliceH k0 l hl a0_ t ht
  -- k0 = k1
  EQ ->                case u hl0 l0 hl1 l1 of
        (# l,hl #)  -> case u hr0 r0 hr1 r1 of
         (# r,hr #) -> let a_ = f a0 a1 in a_ `seq` spliceH k0 l hl a_ r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                                 case forkL k0 a0 hr1 r1 of
        (# hrl1,rl1,a0_,hrr1,rr1 #)  -> case forkR hl0 l0 k1 a1 of -- (k1  < rl1 < k0) & (k1 < k0  < rr1)
         (# hll0,ll0,a1_,hlr0,lr0 #) ->                            -- (ll0 < k1  < k0) & (k1 < lr0 < k0)
          -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                        case u hll0 ll0  hl1  l1 of
          (# l,hl #)                 -> case u hlr0 lr0 hrl1 rl1 of
           (# m,hm #)                -> case u  hr0  r0 hrr1 rr1 of
            (# r,hr #)               -> case spliceH k1 l hl a1_ m hm of
             (# t,ht #)              -> spliceH k0 t ht a0_ r hr
 -- We need 2 different versions of fork (L & R) to ensure that values are combined in
 -- the right order (f a0 a1)
 ------------------------------------------------
 -- forkL :: IntKey -> a -> Int# -> IntMap a -> (# Int#,IntMap a,a,Int#,IntMap a #)
 forkL k0 a0 ht1 t1 = forkL_ ht1 t1 where
  forkL_ h  E          = (# h,E,a0,h,E #)
  forkL_ h (N k l a r) = forkL__ k ((h)-#2#) l a ((h)-#1#) r
  forkL_ h (Z k l a r) = forkL__ k ((h)-#1#) l a ((h)-#1#) r
  forkL_ h (P k l a r) = forkL__ k ((h)-#1#) l a ((h)-#2#) r
  forkL__ k hl l a hr r = case compareInt# k0 k of
                          LT ->                            case forkL_ hl l of
                                (# hl0,l0,a0_,hl1,l1 #) -> case spliceH k l1 hl1 a r hr of
                                 (# l1_,hl1_ #)         -> (# hl0,l0,a0_,hl1_,l1_ #)
                          EQ ->                            let a_ = f a0 a in a_ `seq`
                                                           (# hl,l,a_,hr,r #)
                          GT ->                            case forkL_ hr r of
                                (# hl0,l0,a0_,hl1,l1 #) -> case spliceH k l hl a l0 hl0 of
                                 (# l0_,hl0_ #)         -> (# hl0_,l0_,a0_,hl1,l1 #)
 ------------------------------------------------
 -- forkL :: Int# -> IntMap a -> IntKey -> a -> (# Int#,IntMap a,a,Int#,IntMap a #)
 forkR ht0 t0 k1 a1 = forkR_ ht0 t0 where
  forkR_ h  E          = (# h,E,a1,h,E #)
  forkR_ h (N k l a r) = forkR__ k ((h)-#2#) l a ((h)-#1#) r
  forkR_ h (Z k l a r) = forkR__ k ((h)-#1#) l a ((h)-#1#) r
  forkR_ h (P k l a r) = forkR__ k ((h)-#1#) l a ((h)-#2#) r
  forkR__ k hl l a hr r = case compareInt# k k1 of
                          LT ->                            case forkR_ hr r of
                                (# hl0,l0,a1_,hl1,l1 #) -> case spliceH k l hl a l0 hl0 of
                                 (# l0_,hl0_ #)         -> (# hl0_,l0_,a1_,hl1,l1 #)
                          EQ ->                            let a_ = f a a1 in a_ `seq`
                                                           (# hl,l,a_,hr,r #)
                          GT ->                            case forkR_ hl l of
                                (# hl0,l0,a1_,hl1,l1 #) -> case spliceH k l1 hl1 a r hr of
                                 (# l1_,hl1_ #)         -> (# hl0,l0,a1_,hl1_,l1_ #)
 ------------------------------------------------
 -- pushAB :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB k0 a0 ht1 t1 = pushH' (\a1 -> f a0 a1) k0 a0 ht1 t1
 ------------------------------------------------
 -- pushBA :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA k1 a1 ht0 t0 = pushH' (\a0 -> f a0 a1) k1 a1 ht0 t0
 ------------------------------------------------
 -- pushAB2 :: IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB2 k0 a0 k0_ a0_ ht1 t1 = case pushAB k0_ a0_ ht1 t1 of
                                (# t,h #) -> pushAB k0 a0 h t
 ------------------------------------------------
 -- pushBA2 :: IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA2 k1 a1 k1_ a1_ ht0 t0 = case pushBA k1_ a1_ ht0 t0 of
                                (# t,h #) -> pushBA k1 a1 h t
 ------------------------------------------------
 -- pushAB3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB3 k0 a0 k0_ a0_ k0__ a0__ ht1 t1 = case pushAB k0__ a0__ ht1 t1 of
                                          (# t,h #) -> pushAB2 k0 a0 k0_ a0_ h t
 ------------------------------------------------
 -- pushBA3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA3 k1 a1 k1_ a1_ k1__ a1__ ht0 t0 = case pushBA k1__ a1__ ht0 t0 of
                                          (# t,h #) -> pushBA2 k1 a1 k1_ a1_ h t
-----------------------------------------------------------------------
----------------------- unionIntMap' Ends Here --------------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'unionMaybe'.
unionMaybeIntMap :: (a -> a -> Maybe a) -> IntMap a -> IntMap a -> IntMap a
unionMaybeIntMap f t0_ t1_ = u0 t0_ t1_ where
 u0     E            t1               = t1
 u0 t0                   E            = t0
 u0 t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = uH (addHeight 2# l0) t0 (addHeight 2# l1) t1
 u0 t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = uH (addHeight 2# l0) t0 (addHeight 1# l1) t1
 u0 t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = uH (addHeight 2# l0) t0 (addHeight 2# r1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = uH (addHeight 1# l0) t0 (addHeight 2# l1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = uH (addHeight 1# l0) t0 (addHeight 1# l1) t1
 u0 t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = uH (addHeight 1# l0) t0 (addHeight 2# r1) t1
 u0 t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = uH (addHeight 2# r0) t0 (addHeight 2# l1) t1
 u0 t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = uH (addHeight 2# r0) t0 (addHeight 1# l1) t1
 u0 t0@(P _ _  _ r0) t1@(P _ _  _ r1) = uH (addHeight 2# r0) t0 (addHeight 2# r1) t1
 -- uH :: Int# -> IntMap a ->   -- 1st IntMap with height
 --       Int# -> IntMap a ->   -- 2nd IntMap with height
 --       IntMap a
 uH h0 t0 h1 t1 = case u h0 t0 h1 t1 of (# t,_ #) -> t
 -- u :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --      Int# -> IntMap a  ->    -- 2nd IntMap with height
 --      (# Int#,IntMap a #)     -- Output IntMap with height
 ------------------------------------------------
 u 0# _    h1              t1              = (# t1,h1 #)
 u h0   t0   0#            _               = (# t0,h0 #)
 ------------------------------------------------
 u 1# (Z k0 _  a0 _ ) 1# t1@(Z k1 _  a1 _ ) = case compareInt# k0 k1 of
                                                  LT -> (# N k0  E  a0 t1, 2# #)
                                                  EQ ->  case f a0 a1 of
                                                         Just a  -> (# Z k0 E a E , 1# #)
                                                         Nothing -> (# E          , 0# #)
                                                  GT -> (# P k0  t1 a0 E , 2# #)
 u 1# (Z k0 _  a0 _ ) ht1  t1              = pushAB k0 a0 ht1 t1
 u ht0  t0              1# (Z k1 _  a1 _ ) = pushBA k1 a1 ht0 t0
 ------------------------------------------------
 u 2# (N k0 _ a0 (Z k0_ _ a0_ _)) ht1 t1 = pushAB2 k0 a0 k0_ a0_ ht1 t1
 u 2# (P k0_ (Z k0 _ a0 _) a0_ _) ht1 t1 = pushAB2 k0 a0 k0_ a0_ ht1 t1
 u ht0 t0 2# (N k1 _ a1 (Z k1_ _ a1_ _)) = pushBA2 k1 a1 k1_ a1_ ht0 t0
 u ht0 t0 2# (P k1_ (Z k1 _ a1 _) a1_ _) = pushBA2 k1 a1 k1_ a1_ ht0 t0
 u 2# (Z k0_ (Z k0 _ a0 _) a0_ (Z k0__ _ a0__ _)) ht1 t1 = pushAB3 k0 a0 k0_ a0_ k0__ a0__ ht1 t1
 u ht0 t0 2# (Z k1_ (Z k1 _ a1 _) a1_ (Z k1__ _ a1__ _)) = pushBA3 k1 a1 k1_ a1_ k1__ a1__ ht0 t0
 ------------------------------------------------
 u h0 (N k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (N k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (N k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#2#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u h0 (Z k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (Z k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (Z k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#1#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u h0 (P k0 l0 a0 r0) h1 (N k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#2#) l1 a1 ((h1)-#1#) r1
 u h0 (P k0 l0 a0 r0) h1 (Z k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#1#) r1
 u h0 (P k0 l0 a0 r0) h1 (P k1 l1 a1 r1) = u_ k0 ((h0)-#1#) l0 a0 ((h0)-#2#) r0 k1 ((h1)-#1#) l1 a1 ((h1)-#2#) r1
 u _  _               _  _               = error (mErr ++ "unionMaybeIntMap: Bad IntMap.")
 u_ k0 hl0 l0 a0 hr0 r0 k1 hl1 l1 a1 hr1 r1 =
  case compareInt# k0 k1 of
  -- k0 < k1, so (l0 < k0 < k1) & (k0 < k1 < r1)
  LT ->                                  case forkR hr0 r0 k1 a1 of
        (# hrl0,rl0,mba1,hrr0,rr0 #)  -> case forkL k0 a0 hl1 l1 of -- (k0  < rl0 < k1) & (k0 < k1  < rr0)
         (# hll1,ll1,mba0,hlr1,lr1 #) ->                            -- (ll1 < k0  < k1) & (k0 < lr1 < k1)
          -- (l0 + ll1) < k0 < (rl0 + lr1) < k1 < (rr0 + r1)
                                         case u  hl0  l0 hll1 ll1 of
          (# l,hl #)                  -> case u hrl0 rl0 hlr1 lr1 of
           (# m,hm #)                 -> case u hrr0 rr0  hr1  r1 of
            (# r,hr #)                -> case (case mba1 of Just a  -> spliceH k1 m hm a r hr
                                                            Nothing -> joinH      m hm   r hr
                                              ) of
             (# t,ht #)               -> case mba0 of Just a  -> spliceH k0 l hl a t ht
                                                      Nothing -> joinH      l hl   t ht
  -- k0 = k1
  EQ ->                case u hl0 l0 hl1 l1 of
        (# l,hl #)  -> case u hr0 r0 hr1 r1 of
         (# r,hr #) -> case f a0 a1 of Just a  -> spliceH k0 l hl a r hr
                                       Nothing -> joinH      l hl   r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                                  case forkL k0 a0 hr1 r1 of
        (# hrl1,rl1,mba0,hrr1,rr1 #)  -> case forkR hl0 l0 k1 a1 of -- (k1  < rl1 < k0) & (k1 < k0  < rr1)
         (# hll0,ll0,mba1,hlr0,lr0 #) ->                            -- (ll0 < k1  < k0) & (k1 < lr0 < k0)
          -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                         case u hll0 ll0  hl1  l1 of
          (# l,hl #)                  -> case u hlr0 lr0 hrl1 rl1 of
           (# m,hm #)                 -> case u  hr0  r0 hrr1 rr1 of
            (# r,hr #)                -> case (case mba1 of Just a  -> spliceH k1 l hl a m hm
                                                            Nothing -> joinH      l hl   m hm
                                              ) of
             (# t,ht #)               -> case mba0 of Just a  -> spliceH k0 t ht a r hr
                                                      Nothing -> joinH      t ht   r hr
 -- We need 2 different versions of fork (L & R) to ensure that values are combined in
 -- the right order (f a0 a1)
 ------------------------------------------------
 -- forkL :: IntKey -> a -> Int# -> IntMap a -> (# Int#,IntMap a,Maybe a,Int#,IntMap a #)
 forkL k0 a0 ht1 t1 = forkL_ ht1 t1 where
  forkL_ h  E          = (# h,E,Just a0,h,E #)
  forkL_ h (N k l a r) = forkL__ k ((h)-#2#) l a ((h)-#1#) r
  forkL_ h (Z k l a r) = forkL__ k ((h)-#1#) l a ((h)-#1#) r
  forkL_ h (P k l a r) = forkL__ k ((h)-#1#) l a ((h)-#2#) r
  forkL__ k hl l a hr r = case compareInt# k0 k of
                          LT ->                            case forkL_ hl l of
                                (# hl0,l0,a0_,hl1,l1 #) -> case spliceH k l1 hl1 a r hr of
                                 (# l1_,hl1_ #)         -> (# hl0,l0,a0_,hl1_,l1_ #)
                          EQ -> let mba = f a0 a in mba `seq` (# hl,l,mba,hr,r #)
                          GT ->                            case forkL_ hr r of
                                (# hl0,l0,a0_,hl1,l1 #) -> case spliceH k l hl a l0 hl0 of
                                 (# l0_,hl0_ #)         -> (# hl0_,l0_,a0_,hl1,l1 #)
 ------------------------------------------------
 -- forkL :: Int# -> IntMap a -> IntKey -> a -> (# Int#,IntMap a,Maybe a,Int#,IntMap a #)
 forkR ht0 t0 k1 a1 = forkR_ ht0 t0 where
  forkR_ h  E          = (# h,E,Just a1,h,E #)
  forkR_ h (N k l a r) = forkR__ k ((h)-#2#) l a ((h)-#1#) r
  forkR_ h (Z k l a r) = forkR__ k ((h)-#1#) l a ((h)-#1#) r
  forkR_ h (P k l a r) = forkR__ k ((h)-#1#) l a ((h)-#2#) r
  forkR__ k hl l a hr r = case compareInt# k k1 of
                          LT ->                            case forkR_ hr r of
                                (# hl0,l0,a1_,hl1,l1 #) -> case spliceH k l hl a l0 hl0 of
                                 (# l0_,hl0_ #)         -> (# hl0_,l0_,a1_,hl1,l1 #)
                          EQ -> let mba = f a a1 in mba `seq` (# hl,l,mba,hr,r #)
                          GT ->                            case forkR_ hl l of
                                (# hl0,l0,a1_,hl1,l1 #) -> case spliceH k l1 hl1 a r hr of
                                 (# l1_,hl1_ #)         -> (# hl0,l0,a1_,hl1_,l1_ #)
 ------------------------------------------------
 -- pushAB :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB k0 a0 ht1 t1 = pushMaybeH (\a1 -> f a0 a1) k0 a0 ht1 t1
 ------------------------------------------------
 -- pushBA :: IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA k1 a1 ht0 t0 = pushMaybeH (\a0 -> f a0 a1) k1 a1 ht0 t0
 ------------------------------------------------
 -- pushAB2 :: IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB2 k0 a0 k0_ a0_ ht1 t1 = case pushAB k0_ a0_ ht1 t1 of
                                (# t,h #) -> pushAB k0 a0 h t
 ------------------------------------------------
 -- pushBA2 :: IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA2 k1 a1 k1_ a1_ ht0 t0 = case pushBA k1_ a1_ ht0 t0 of
                                (# t,h #) -> pushBA k1 a1 h t
 ------------------------------------------------
 -- pushAB3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushAB3 k0 a0 k0_ a0_ k0__ a0__ ht1 t1 = case pushAB k0__ a0__ ht1 t1 of
                                          (# t,h #) -> pushAB2 k0 a0 k0_ a0_ h t
 ------------------------------------------------
 -- pushBA3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
 pushBA3 k1 a1 k1_ a1_ k1__ a1__ ht0 t0 = case pushBA k1__ a1__ ht0 t0 of
                                          (# t,h #) -> pushBA2 k1 a1 k1_ a1_ h t
-----------------------------------------------------------------------
-------------------- unionMaybeIntMap Ends Here ------------------------
-----------------------------------------------------------------------

-- Utility used by unionMaybeIntMap
pushMaybeH :: (a -> Maybe a) -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a,Int# #)
pushMaybeH f k0 a0 ht1 t1 = case lookupIntMap k0 t1 of
                            Nothing -> insH k0 a0 ht1 t1
                            Just a  -> case f a of
                                       Nothing -> delH k0 ht1 t1
                                       Just a_ -> let t_ = assertWriteIntMap k0 a_ t1 in t_ `seq`
                                                  (# t_,ht1 #) -- No height change

-- -- Utility used by unionMaybeIntMap
-- pushMaybeH' :: (a -> Maybe a) -> IntKey -> a -> Int# -> IntMap a -> (# IntMap a, Int# #)
-- pushMaybeH' f k0 a0 ht1 t1 = case lookupIntMap k0 t1 of
--                             Nothing -> insH k0 a0 ht1 t1
--                             Just a  -> case f a of
--                                        Nothing -> delH k0 ht1 t1
--                                        Just a_ -> a_ `seq` let t_ = assertWriteIntMap k0 a_ t1 in t_ `seq`
--                                                   (# t_,ht1 #) -- No height change

-- | Specialised association list.
data IAList a = Empt
              | Cons {-# UNPACK #-} !Int# a (IAList a)
              deriving(Eq,Ord)

-- | Convert an 'IntMap' to an 'IAList' (in ascending order).
asIAList :: IntMap a -> IAList a
asIAList imp = f imp Empt where
 f  E          ial = ial
 f (N k l a r) ial = f' k l a r ial
 f (Z k l a r) ial = f' k l a r ial
 f (P k l a r) ial = f' k l a r ial
 f' k l a r ial = let ial'  = f r ial
                      ial'' = ial' `seq` Cons k a ial'
                  in ial'' `seq` f l ial''

-- | See 'Map' class method 'intersection'.
intersectionIntMap :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionIntMap f ta0 tb0 = i0 ta0 tb0 where
 -- i0 :: IntMap a -> IntMap b -> IntMap c
 i0     E            _                = E
 i0 _                    E            = E
 i0 ta@(N _ la _ _ ) tb@(N _ lb _ _ ) = iH (addHeight 2# la) ta (addHeight 2# lb) tb
 i0 ta@(N _ la _ _ ) tb@(Z _ lb _ _ ) = iH (addHeight 2# la) ta (addHeight 1# lb) tb
 i0 ta@(N _ la _ _ ) tb@(P _ _  _ rb) = iH (addHeight 2# la) ta (addHeight 2# rb) tb
 i0 ta@(Z _ la _ _ ) tb@(N _ lb _ _ ) = iH (addHeight 1# la) ta (addHeight 2# lb) tb
 i0 ta@(Z _ la _ _ ) tb@(Z _ lb _ _ ) = iH (addHeight 1# la) ta (addHeight 1# lb) tb
 i0 ta@(Z _ la _ _ ) tb@(P _ _  _ rb) = iH (addHeight 1# la) ta (addHeight 2# rb) tb
 i0 ta@(P _ _  _ ra) tb@(N _ lb _ _ ) = iH (addHeight 2# ra) ta (addHeight 2# lb) tb
 i0 ta@(P _ _  _ ra) tb@(Z _ lb _ _ ) = iH (addHeight 2# ra) ta (addHeight 1# lb) tb
 i0 ta@(P _ _  _ ra) tb@(P _ _  _ rb) = iH (addHeight 2# ra) ta (addHeight 2# rb) tb

 -- iH :: Int# -> IntMap a ->   -- 1st IntMap with height
 --       Int# -> IntMap b ->   -- 2nd IntMap with height
 --       IntMap c
 iH hta ta htb tb  = case i hta ta htb tb Empt 0# of
                     (# ial,n #)   -> case subst (rep (I# (n))) ial of
                      (# imp,rm #) -> case rm of
                                      Empt -> imp
                                      _    -> error (mErr ++ "intersectionIntMap: Bad IAList.")

 -- i :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --      Int# -> IntMap b  ->    -- 2nd IntMap with height
 --      IAList c -> Int# ->    -- Input IAList with length
 --      (# IAList c, Int# #)   -- Output IAList with length
 ------------------------------------------------
 i 0# _ _    _ cs n = (# cs,n #)
 i _    _ 0# _ cs n = (# cs,n #)
 ------------------------------------------------
 i 1# (Z ka _  ea _ ) 1# (Z kb _  eb _ ) cs n = if ka ==# kb then (# Cons ka (f ea eb) cs, ((n)+#1#) #)
                                                                 else (# cs,n #)
 i 1# (Z ka _  ea _ ) _    tb              cs n = lookAB ka ea tb cs n
 i _    ta              1# (Z kb _  eb _ ) cs n = lookBA kb eb ta cs n
 ------------------------------------------------
 i 2# (N ka0 _               ea0 (Z ka1 _ ea1 _)) _ tb cs n = lookAB2 ka0 ea0 ka1 ea1 tb cs n
 i 2# (P ka1 (Z ka0 _ ea0 _) ea1 _              ) _ tb cs n = lookAB2 ka0 ea0 ka1 ea1 tb cs n
 i _ ta 2# (N kb0 _               eb0 (Z kb1 _ eb1 _)) cs n = lookBA2 kb0 eb0 kb1 eb1 ta cs n
 i _ ta 2# (P kb1 (Z kb0 _ eb0 _) eb1 _              ) cs n = lookBA2 kb0 eb0 kb1 eb1 ta cs n
 i 2# (Z ka1 (Z ka0 _ ea0 _) ea1 (Z ka2 _ ea2 _)) _ tb cs n = lookAB3 ka0 ea0 ka1 ea1 ka2 ea2 tb cs n
 i _ ta 2# (Z kb1 (Z kb0 _ eb0 _) eb1 (Z kb2 _ eb2 _)) cs n = lookBA3 kb0 eb0 kb1 eb1 kb2 eb2 ta cs n
 ------------------------------------------------
 -- Both tree heights are known to be >= 3 at this point, so sub-tree heights >= 1
 i ha (N ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (N ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (N ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i ha (Z ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (Z ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (Z ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i ha (P ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (P ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (P ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i _  _               _  _               _  _ = error (mErr ++ "intersectionIntMap: Bad IntMap.")
 ------------------------------------------------
 i_ ka hla la ea hra ra kb hlb lb eb hrb rb cs n = case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT                            -> case fork kb hra ra of
   (# hrla,rla,mba,hrra,rra #)  -> case fork ka hlb lb of         -- (ka  < rla < kb) & (ka < kb  < rra)
    (# hllb,llb,mbb,hlrb,lrb #) -> case i hrra rra hrb rb cs n of -- (llb < ka  < kb) & (ka < lrb < kb)
     -- (la + llb) < ka < (rla + lrb) < kb < (rra + rb)
     (# cs_,n_ #)               -> case (case mbb of
                                         Nothing -> i hrla rla hlrb lrb cs_                    n_
                                         Just b  -> i hrla rla hlrb lrb (Cons ka (f ea b) cs_) ((n_)+#1#)
                                        ) of
      (# cs__,n__ #)            -> case mba of
                                   Nothing -> i hla la hllb llb cs__                    n__
                                   Just a  -> i hla la hllb llb (Cons kb (f a eb) cs__) ((n__)+#1#)
  -- ka = kb
  EQ                            -> case i hra ra hrb rb cs n of
   (# cs_,n_ #)                 -> i hla la hlb lb (Cons ka (f ea eb) cs_) ((n_)+#1#)
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT                            -> case fork ka hrb rb of
   (# hrlb,rlb,mbb,hrrb,rrb #)  -> case fork kb hla la of         -- (kb  < rlb < ka) & (kb < ka  < rrb)
    (# hlla,lla,mba,hlra,lra #) -> case i hra ra hrrb rrb cs n of -- (lla < kb  < ka) & (kb < lra < ka)
     -- (lla + lb) < kb < (lra + rlb) < ka < (ra + rrb)
     (# cs_,n_ #)               -> case (case mba of
                                         Nothing -> i hlra lra hrlb rlb cs_                    n_
                                         Just a  -> i hlra lra hrlb rlb (Cons kb (f a eb) cs_) ((n_)+#1#)
                                        ) of
      (# cs__,n__ #)           -> case mbb of
                                  Nothing -> i hlla lla hlb lb cs__                    n__
                                  Just b  -> i hlla lla hlb lb (Cons ka (f ea b) cs__) ((n__)+#1#)
 ------------------------------------------------
 -- fork :: IntKey -> Int# -> IntMap x -> (# Int#,IntMap x,Maybe x,Int#,IntMap x #)
 -- Tree height (ht) is known to be >= 1, can we exploit this ??
 fork k0 ht t = fork_ ht t where
  fork_ h  E          = (# h,E,Nothing,h,E #)
  fork_ h (N k l x r) = fork__ k ((h)-#2#) l x ((h)-#1#) r
  fork_ h (Z k l x r) = fork__ k ((h)-#1#) l x ((h)-#1#) r
  fork_ h (P k l x r) = fork__ k ((h)-#1#) l x ((h)-#2#) r
  fork__ k hl l x hr r = case compareInt# k0 k of
                         LT ->                            case fork_ hl l of
                               (# hl0,l0,mbx,hl1,l1 #) -> case spliceH k l1 hl1 x r hr of
                                (# l1_,hl1_ #)         -> (# hl0,l0,mbx,hl1_,l1_ #)
                         EQ -> (# hl,l,Just x,hr,r #)
                         GT ->                            case fork_ hr r of
                               (# hl0,l0,mbx,hl1,l1 #) -> case spliceH k l hl x l0 hl0 of
                                (# l0_,hl0_ #)         -> (# hl0_,l0_,mbx,hl1,l1 #)
 ------------------------------------------------
 -- lookAB :: IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB ka ea tb cs n = rd tb where
  rd  E          = (# cs,n #)
  rd (N k l b r) = rd_ k l b r
  rd (Z k l b r) = rd_ k l b r
  rd (P k l b r) = rd_ k l b r
  rd_   k l b r  = case compareInt# ka k of
                   LT -> rd l
                   EQ -> (# Cons ka (f ea b) cs, ((n)+#1#) #)
                   GT -> rd r
 ------------------------------------------------
 -- lookBA :: IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA kb eb ta cs n = rd ta where
  rd  E          = (# cs,n #)
  rd (N k l a r) = rd_ k l a r
  rd (Z k l a r) = rd_ k l a r
  rd (P k l a r) = rd_ k l a r
  rd_   k l a r  = case compareInt# kb k of
                   LT -> rd l
                   EQ -> (# Cons kb (f a eb) cs, ((n)+#1#) #)
                   GT -> rd r
 ------------------------------------------------
 -- lookAB2 :: IntKey -> a -> IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB2 ka0 ea0 ka1 ea1 tb cs n = case lookAB ka1 ea1 tb cs n of
                                   (# cs_,n_ #) -> lookAB ka0 ea0 tb cs_ n_
 ------------------------------------------------
 -- lookBA2 :: IntKey -> b -> IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA2 kb0 eb0 kb1 eb1 ta cs n = case lookBA kb1 eb1 ta cs n of
                                   (# cs_,n_ #) -> lookBA kb0 eb0 ta cs_ n_
 ------------------------------------------------
 -- lookAB3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB3 ka0 ea0 ka1 ea1 ka2 ea2 tb cs n = case lookAB ka2 ea2 tb cs n of
                                           (# cs_,n_ #) -> lookAB2 ka0 ea0 ka1 ea1 tb cs_ n_
 ------------------------------------------------
 -- lookAB3 :: IntKey -> b -> IntKey -> b -> IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA3 kb0 eb0 kb1 eb1 kb2 eb2 ta cs n = case lookBA kb2 eb2 ta cs n of
                                           (# cs_,n_ #) -> lookBA2 kb0 eb0 kb1 eb1 ta cs_ n_
-----------------------------------------------------------------------
-------------------- intersectionIntMap Ends Here ----------------------
-----------------------------------------------------------------------


-- | See 'Map' class method 'intersection''.
intersectionIntMap' :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionIntMap' f ta0 tb0 = i0 ta0 tb0 where
 -- i0 :: IntMap a -> IntMap b -> IntMap c
 i0     E            _                = E
 i0 _                    E            = E
 i0 ta@(N _ la _ _ ) tb@(N _ lb _ _ ) = iH (addHeight 2# la) ta (addHeight 2# lb) tb
 i0 ta@(N _ la _ _ ) tb@(Z _ lb _ _ ) = iH (addHeight 2# la) ta (addHeight 1# lb) tb
 i0 ta@(N _ la _ _ ) tb@(P _ _  _ rb) = iH (addHeight 2# la) ta (addHeight 2# rb) tb
 i0 ta@(Z _ la _ _ ) tb@(N _ lb _ _ ) = iH (addHeight 1# la) ta (addHeight 2# lb) tb
 i0 ta@(Z _ la _ _ ) tb@(Z _ lb _ _ ) = iH (addHeight 1# la) ta (addHeight 1# lb) tb
 i0 ta@(Z _ la _ _ ) tb@(P _ _  _ rb) = iH (addHeight 1# la) ta (addHeight 2# rb) tb
 i0 ta@(P _ _  _ ra) tb@(N _ lb _ _ ) = iH (addHeight 2# ra) ta (addHeight 2# lb) tb
 i0 ta@(P _ _  _ ra) tb@(Z _ lb _ _ ) = iH (addHeight 2# ra) ta (addHeight 1# lb) tb
 i0 ta@(P _ _  _ ra) tb@(P _ _  _ rb) = iH (addHeight 2# ra) ta (addHeight 2# rb) tb

 -- iH :: Int# -> IntMap a ->   -- 1st IntMap with height
 --       Int# -> IntMap b ->   -- 2nd IntMap with height
 --       IntMap c
 iH hta ta htb tb  = case i hta ta htb tb Empt 0# of
                     (# ial,n #)   -> case subst (rep (I# (n))) ial of
                      (# imp,rm #) -> case rm of
                                      Empt -> imp
                                      _    -> error (mErr ++ "intersectionIntMap': Bad IAList.")

 -- i :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --      Int# -> IntMap b  ->    -- 2nd IntMap with height
 --      IAList c -> Int# ->    -- Input IAList with length
 --      (# IAList c, Int# #)   -- Output IAList with length
 ------------------------------------------------
 i 0# _ _    _ cs n = (# cs,n #)
 i _    _ 0# _ cs n = (# cs,n #)
 ------------------------------------------------
 i 1# (Z ka _  ea _ ) 1# (Z kb _  eb _ ) cs n = if ka ==# kb then let c = f ea eb in c `seq`
                                                                      (# Cons ka c cs, ((n)+#1#) #)
                                                                 else (# cs,n #)
 i 1# (Z ka _  ea _ ) _    tb              cs n = lookAB ka ea tb cs n
 i _    ta              1# (Z kb _  eb _ ) cs n = lookBA kb eb ta cs n
 ------------------------------------------------
 i 2# (N ka0 _               ea0 (Z ka1 _ ea1 _)) _ tb cs n = lookAB2 ka0 ea0 ka1 ea1 tb cs n
 i 2# (P ka1 (Z ka0 _ ea0 _) ea1 _              ) _ tb cs n = lookAB2 ka0 ea0 ka1 ea1 tb cs n
 i _ ta 2# (N kb0 _               eb0 (Z kb1 _ eb1 _)) cs n = lookBA2 kb0 eb0 kb1 eb1 ta cs n
 i _ ta 2# (P kb1 (Z kb0 _ eb0 _) eb1 _              ) cs n = lookBA2 kb0 eb0 kb1 eb1 ta cs n
 i 2# (Z ka1 (Z ka0 _ ea0 _) ea1 (Z ka2 _ ea2 _)) _ tb cs n = lookAB3 ka0 ea0 ka1 ea1 ka2 ea2 tb cs n
 i _ ta 2# (Z kb1 (Z kb0 _ eb0 _) eb1 (Z kb2 _ eb2 _)) cs n = lookBA3 kb0 eb0 kb1 eb1 kb2 eb2 ta cs n
 ------------------------------------------------
 -- Both tree heights are known to be >= 3 at this point, so sub-tree heights >= 1
 i ha (N ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (N ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (N ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i ha (Z ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (Z ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (Z ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i ha (P ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (P ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (P ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i _  _               _  _               _  _ = error (mErr ++ "intersectionIntMap': Bad IntMap.")
 ------------------------------------------------
 i_ ka hla la ea hra ra kb hlb lb eb hrb rb cs n = case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT                            -> case fork kb hra ra of
   (# hrla,rla,mba,hrra,rra #)  -> case fork ka hlb lb of         -- (ka  < rla < kb) & (ka < kb  < rra)
    (# hllb,llb,mbb,hlrb,lrb #) -> case i hrra rra hrb rb cs n of -- (llb < ka  < kb) & (ka < lrb < kb)
     -- (la + llb) < ka < (rla + lrb) < kb < (rra + rb)
     (# cs_,n_ #)               -> case (case mbb of
                                         Nothing -> i hrla rla hlrb lrb cs_             n_
                                         Just b  -> let c = f ea b in c `seq`
                                                    i hrla rla hlrb lrb (Cons ka c cs_) ((n_)+#1#)
                                        ) of
      (# cs__,n__ #)            -> case mba of
                                   Nothing -> i hla la hllb llb cs__             n__
                                   Just a  -> let c = f a eb in c `seq`
                                              i hla la hllb llb (Cons kb c cs__) ((n__)+#1#)
  -- ka = kb
  EQ                            -> case i hra ra hrb rb cs n of
   (# cs_,n_ #)                 -> let c = f ea eb in c `seq`
                                   i hla la hlb lb (Cons ka c cs_) ((n_)+#1#)
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT                            -> case fork ka hrb rb of
   (# hrlb,rlb,mbb,hrrb,rrb #)  -> case fork kb hla la of         -- (kb  < rlb < ka) & (kb < ka  < rrb)
    (# hlla,lla,mba,hlra,lra #) -> case i hra ra hrrb rrb cs n of -- (lla < kb  < ka) & (kb < lra < ka)
     -- (lla + lb) < kb < (lra + rlb) < ka < (ra + rrb)
     (# cs_,n_ #)               -> case (case mba of
                                         Nothing -> i hlra lra hrlb rlb cs_             n_
                                         Just a  -> let c = f a eb in c `seq`
                                                    i hlra lra hrlb rlb (Cons kb c cs_) ((n_)+#1#)
                                        ) of
      (# cs__,n__ #)           -> case mbb of
                                  Nothing -> i hlla lla hlb lb cs__             n__
                                  Just b  -> let c = f ea b in c `seq`
                                             i hlla lla hlb lb (Cons ka c cs__) ((n__)+#1#)
 ------------------------------------------------
 -- fork :: IntKey -> Int# -> IntMap x -> (# Int#,IntMap x,Maybe x,Int#,IntMap x #)
 -- Tree height (ht) is known to be >= 1, can we exploit this ??
 fork k0 ht t = fork_ ht t where
  fork_ h  E          = (# h,E,Nothing,h,E #)
  fork_ h (N k l x r) = fork__ k ((h)-#2#) l x ((h)-#1#) r
  fork_ h (Z k l x r) = fork__ k ((h)-#1#) l x ((h)-#1#) r
  fork_ h (P k l x r) = fork__ k ((h)-#1#) l x ((h)-#2#) r
  fork__ k hl l x hr r = case compareInt# k0 k of
                         LT ->                            case fork_ hl l of
                               (# hl0,l0,mbx,hl1,l1 #) -> case spliceH k l1 hl1 x r hr of
                                (# l1_,hl1_ #)         -> (# hl0,l0,mbx,hl1_,l1_ #)
                         EQ -> (# hl,l,Just x,hr,r #)
                         GT ->                            case fork_ hr r of
                               (# hl0,l0,mbx,hl1,l1 #) -> case spliceH k l hl x l0 hl0 of
                                (# l0_,hl0_ #)         -> (# hl0_,l0_,mbx,hl1,l1 #)
 ------------------------------------------------
 -- lookAB :: IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB ka ea tb cs n = rd tb where
  rd  E          = (# cs,n #)
  rd (N k l b r) = rd_ k l b r
  rd (Z k l b r) = rd_ k l b r
  rd (P k l b r) = rd_ k l b r
  rd_   k l b r  = case compareInt# ka k of
                   LT -> rd l
                   EQ -> let c = f ea b in c `seq` (# Cons ka c cs, ((n)+#1#) #)
                   GT -> rd r
 ------------------------------------------------
 -- lookBA :: IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA kb eb ta cs n = rd ta where
  rd  E          = (# cs,n #)
  rd (N k l a r) = rd_ k l a r
  rd (Z k l a r) = rd_ k l a r
  rd (P k l a r) = rd_ k l a r
  rd_   k l a r  = case compareInt# kb k of
                   LT -> rd l
                   EQ -> let c = f a eb in c `seq` (# Cons kb c cs, ((n)+#1#) #)
                   GT -> rd r
 ------------------------------------------------
 -- lookAB2 :: IntKey -> a -> IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB2 ka0 ea0 ka1 ea1 tb cs n = case lookAB ka1 ea1 tb cs n of
                                   (# cs_,n_ #) -> lookAB ka0 ea0 tb cs_ n_
 ------------------------------------------------
 -- lookBA2 :: IntKey -> b -> IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA2 kb0 eb0 kb1 eb1 ta cs n = case lookBA kb1 eb1 ta cs n of
                                   (# cs_,n_ #) -> lookBA kb0 eb0 ta cs_ n_
 ------------------------------------------------
 -- lookAB3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB3 ka0 ea0 ka1 ea1 ka2 ea2 tb cs n = case lookAB ka2 ea2 tb cs n of
                                           (# cs_,n_ #) -> lookAB2 ka0 ea0 ka1 ea1 tb cs_ n_
 ------------------------------------------------
 -- lookAB3 :: IntKey -> b -> IntKey -> b -> IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA3 kb0 eb0 kb1 eb1 kb2 eb2 ta cs n = case lookBA kb2 eb2 ta cs n of
                                           (# cs_,n_ #) -> lookBA2 kb0 eb0 kb1 eb1 ta cs_ n_
-----------------------------------------------------------------------
-------------------- intersectionIntMap' Ends Here ---------------------
-----------------------------------------------------------------------


-- | See 'Map' class method 'intersectionMaybe'.
intersectionMaybeIntMap :: (a -> b -> Maybe c) -> IntMap a -> IntMap b -> IntMap c
intersectionMaybeIntMap f ta0 tb0 = i0 ta0 tb0 where
 -- i0 :: IntMap a -> IntMap b -> IntMap c
 i0     E            _                = E
 i0 _                    E            = E
 i0 ta@(N _ la _ _ ) tb@(N _ lb _ _ ) = iH (addHeight 2# la) ta (addHeight 2# lb) tb
 i0 ta@(N _ la _ _ ) tb@(Z _ lb _ _ ) = iH (addHeight 2# la) ta (addHeight 1# lb) tb
 i0 ta@(N _ la _ _ ) tb@(P _ _  _ rb) = iH (addHeight 2# la) ta (addHeight 2# rb) tb
 i0 ta@(Z _ la _ _ ) tb@(N _ lb _ _ ) = iH (addHeight 1# la) ta (addHeight 2# lb) tb
 i0 ta@(Z _ la _ _ ) tb@(Z _ lb _ _ ) = iH (addHeight 1# la) ta (addHeight 1# lb) tb
 i0 ta@(Z _ la _ _ ) tb@(P _ _  _ rb) = iH (addHeight 1# la) ta (addHeight 2# rb) tb
 i0 ta@(P _ _  _ ra) tb@(N _ lb _ _ ) = iH (addHeight 2# ra) ta (addHeight 2# lb) tb
 i0 ta@(P _ _  _ ra) tb@(Z _ lb _ _ ) = iH (addHeight 2# ra) ta (addHeight 1# lb) tb
 i0 ta@(P _ _  _ ra) tb@(P _ _  _ rb) = iH (addHeight 2# ra) ta (addHeight 2# rb) tb

 -- iH :: Int# -> IntMap a ->   -- 1st IntMap with height
 --       Int# -> IntMap b ->   -- 2nd IntMap with height
 --       IntMap c
 iH hta ta htb tb  = case i hta ta htb tb Empt 0# of
                     (# ial,n #)   -> case subst (rep (I# (n))) ial of
                      (# imp,rm #) -> case rm of
                                      Empt -> imp
                                      _    -> error (mErr ++ "intersectionMaybeIntMap: Bad IAList.")

 -- i :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --      Int# -> IntMap b  ->    -- 2nd IntMap with height
 --      IAList c -> Int# ->    -- Input IAList with length
 --      (# IAList c, Int# #)   -- Output IAList with length
 ------------------------------------------------
 i 0# _ _    _ cs n = (# cs,n #)
 i _    _ 0# _ cs n = (# cs,n #)
 ------------------------------------------------
 i 1# (Z ka _  ea _ ) 1# (Z kb _  eb _ ) cs n = if ka ==# kb then case f ea eb of
                                                                      Just c  -> (# Cons ka c cs, ((n)+#1#) #)
                                                                      Nothing -> (# cs,n #)
                                                                 else (# cs,n #)
 i 1# (Z ka _  ea _ ) _    tb              cs n = lookAB ka ea tb cs n
 i _    ta              1# (Z kb _  eb _ ) cs n = lookBA kb eb ta cs n
 ------------------------------------------------
 i 2# (N ka0 _               ea0 (Z ka1 _ ea1 _)) _ tb cs n = lookAB2 ka0 ea0 ka1 ea1 tb cs n
 i 2# (P ka1 (Z ka0 _ ea0 _) ea1 _              ) _ tb cs n = lookAB2 ka0 ea0 ka1 ea1 tb cs n
 i _ ta 2# (N kb0 _               eb0 (Z kb1 _ eb1 _)) cs n = lookBA2 kb0 eb0 kb1 eb1 ta cs n
 i _ ta 2# (P kb1 (Z kb0 _ eb0 _) eb1 _              ) cs n = lookBA2 kb0 eb0 kb1 eb1 ta cs n
 i 2# (Z ka1 (Z ka0 _ ea0 _) ea1 (Z ka2 _ ea2 _)) _ tb cs n = lookAB3 ka0 ea0 ka1 ea1 ka2 ea2 tb cs n
 i _ ta 2# (Z kb1 (Z kb0 _ eb0 _) eb1 (Z kb2 _ eb2 _)) cs n = lookBA3 kb0 eb0 kb1 eb1 kb2 eb2 ta cs n
 ------------------------------------------------
 -- Both tree heights are known to be >= 3 at this point, so sub-tree heights >= 1
 i ha (N ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (N ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (N ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#2#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i ha (Z ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (Z ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (Z ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#1#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i ha (P ka la ea ra) hb (N kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#2#) lb eb ((hb)-#1#) rb cs n
 i ha (P ka la ea ra) hb (Z kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#1#) lb eb ((hb)-#1#) rb cs n
 i ha (P ka la ea ra) hb (P kb lb eb rb) cs n = i_ ka ((ha)-#1#) la ea ((ha)-#2#) ra kb ((hb)-#1#) lb eb ((hb)-#2#) rb cs n
 i _  _               _  _               _  _ = error (mErr ++ "intersectionMaybeIntMap: Bad IntMap.")
 ------------------------------------------------
 i_ ka hla la ea hra ra kb hlb lb eb hrb rb cs n = case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT                            -> case fork kb hra ra of
   (# hrla,rla,mba,hrra,rra #)  -> case fork ka hlb lb of         -- (ka  < rla < kb) & (ka < kb  < rra)
    (# hllb,llb,mbb,hlrb,lrb #) -> case i hrra rra hrb rb cs n of -- (llb < ka  < kb) & (ka < lrb < kb)
     -- (la + llb) < ka < (rla + lrb) < kb < (rra + rb)
     (# cs_,n_ #)               -> case (case mbb of
                                         Nothing ->            i hrla rla hlrb lrb cs_             n_
                                         Just b  -> case f ea b of
                                                    Just c  -> i hrla rla hlrb lrb (Cons ka c cs_) ((n_)+#1#)
                                                    Nothing -> i hrla rla hlrb lrb cs_             n_
                                        ) of
      (# cs__,n__ #)            -> case mba of
                                   Nothing ->            i hla la hllb llb cs__             n__
                                   Just a  -> case f a eb of
                                              Just c  -> i hla la hllb llb (Cons kb c cs__) ((n__)+#1#)
                                              Nothing -> i hla la hllb llb cs__             n__
  -- ka = kb
  EQ                            -> case i hra ra hrb rb cs n of
   (# cs_,n_ #)                 -> case f ea eb of
                                   Just c  -> i hla la hlb lb (Cons ka c cs_) ((n_)+#1#)
                                   Nothing -> i hla la hlb lb cs_             n_
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT                            -> case fork ka hrb rb of
   (# hrlb,rlb,mbb,hrrb,rrb #)  -> case fork kb hla la of         -- (kb  < rlb < ka) & (kb < ka  < rrb)
    (# hlla,lla,mba,hlra,lra #) -> case i hra ra hrrb rrb cs n of -- (lla < kb  < ka) & (kb < lra < ka)
     -- (lla + lb) < kb < (lra + rlb) < ka < (ra + rrb)
     (# cs_,n_ #)               -> case (case mba of
                                         Nothing ->            i hlra lra hrlb rlb cs_             n_
                                         Just a  -> case f a eb of
                                                    Just c  -> i hlra lra hrlb rlb (Cons kb c cs_) ((n_)+#1#)
                                                    Nothing -> i hlra lra hrlb rlb cs_             n_
                                        ) of
      (# cs__,n__ #)           -> case mbb of
                                  Nothing ->            i hlla lla hlb lb cs__             n__
                                  Just b  -> case f ea b of
                                             Just c  -> i hlla lla hlb lb (Cons ka c cs__) ((n__)+#1#)
                                             Nothing -> i hlla lla hlb lb cs__             n__
------------------------------------------------
 -- fork :: IntKey -> Int# -> IntMap x -> (# Int#,IntMap x,Maybe x,Int#,IntMap x #)
 -- Tree height (ht) is known to be >= 1, can we exploit this ??
 fork k0 ht t = fork_ ht t where
  fork_ h  E          = (# h,E,Nothing,h,E #)
  fork_ h (N k l x r) = fork__ k ((h)-#2#) l x ((h)-#1#) r
  fork_ h (Z k l x r) = fork__ k ((h)-#1#) l x ((h)-#1#) r
  fork_ h (P k l x r) = fork__ k ((h)-#1#) l x ((h)-#2#) r
  fork__ k hl l x hr r = case compareInt# k0 k of
                         LT ->                            case fork_ hl l of
                               (# hl0,l0,mbx,hl1,l1 #) -> case spliceH k l1 hl1 x r hr of
                                (# l1_,hl1_ #)         -> (# hl0,l0,mbx,hl1_,l1_ #)
                         EQ -> (# hl,l,Just x,hr,r #)
                         GT ->                            case fork_ hr r of
                               (# hl0,l0,mbx,hl1,l1 #) -> case spliceH k l hl x l0 hl0 of
                                (# l0_,hl0_ #)         -> (# hl0_,l0_,mbx,hl1,l1 #)
 ------------------------------------------------
 -- lookAB :: IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB ka ea tb cs n = rd tb where
  rd  E          = (# cs,n #)
  rd (N k l b r) = rd_ k l b r
  rd (Z k l b r) = rd_ k l b r
  rd (P k l b r) = rd_ k l b r
  rd_   k l b r  = case compareInt# ka k of
                   LT -> rd l
                   EQ -> case f ea b of
                         Just c  -> (# Cons ka c cs, ((n)+#1#) #)
                         Nothing -> (# cs,n #)
                   GT -> rd r
 ------------------------------------------------
 -- lookBA :: IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA kb eb ta cs n = rd ta where
  rd  E          = (# cs,n #)
  rd (N k l a r) = rd_ k l a r
  rd (Z k l a r) = rd_ k l a r
  rd (P k l a r) = rd_ k l a r
  rd_   k l a r  = case compareInt# kb k of
                   LT -> rd l
                   EQ -> case f a eb of
                         Just c  -> (# Cons kb c cs, ((n)+#1#) #)
                         Nothing -> (# cs,n #)
                   GT -> rd r
 ------------------------------------------------
 -- lookAB2 :: IntKey -> a -> IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB2 ka0 ea0 ka1 ea1 tb cs n = case lookAB ka1 ea1 tb cs n of
                                   (# cs_,n_ #) -> lookAB ka0 ea0 tb cs_ n_
 ------------------------------------------------
 -- lookBA2 :: IntKey -> b -> IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA2 kb0 eb0 kb1 eb1 ta cs n = case lookBA kb1 eb1 ta cs n of
                                   (# cs_,n_ #) -> lookBA kb0 eb0 ta cs_ n_
 ------------------------------------------------
 -- lookAB3 :: IntKey -> a -> IntKey -> a -> IntKey -> a -> IntMap b -> IAList c -> Int# -> (# IAList c,Int# #)
 lookAB3 ka0 ea0 ka1 ea1 ka2 ea2 tb cs n = case lookAB ka2 ea2 tb cs n of
                                           (# cs_,n_ #) -> lookAB2 ka0 ea0 ka1 ea1 tb cs_ n_
 ------------------------------------------------
 -- lookAB3 :: IntKey -> b -> IntKey -> b -> IntKey -> b -> IntMap a -> IAList c -> Int# -> (# IAList c,Int# #)
 lookBA3 kb0 eb0 kb1 eb1 kb2 eb2 ta cs n = case lookBA kb2 eb2 ta cs n of
                                           (# cs_,n_ #) -> lookBA2 kb0 eb0 kb1 eb1 ta cs_ n_
-----------------------------------------------------------------------
----------------- intersectionMaybeIntMap Ends Here --------------------
-----------------------------------------------------------------------

-- AVL template, output of rep
data Tmp = ET | NT Tmp Tmp | ZT Tmp Tmp | PT Tmp Tmp
-- Construct a template of size n (n>=0). This is for internal use only.
-- N.B. Uses regular (boxed) Ints. Optimising for unboxed Ints is just too painful in this case.
-- Hopefully the compiler will do a decent job for us...???
rep :: Int -> Tmp
rep n | odd n = repOdd n -- n is odd , >=1
rep n         = repEvn n -- n is even, >=0
-- n is known to be odd (>=1), so left and right sub-trees are identical
repOdd :: Int -> Tmp
repOdd n      = let sub = rep (n `shiftR` 1) in ZT sub sub
-- n is known to be even (>=0)
repEvn :: Int -> Tmp
repEvn n | n .&. (n-1) == 0 = repP2 n -- treat exact powers of 2 specially, traps n=0 too
repEvn n      = let nl = n `shiftR` 1 -- size of left subtree  (odd or even)
                    nr = nl - 1       -- size of right subtree (even or odd)
                in if odd nr
                   then let l = repEvn nl           -- right sub-tree is odd , so left is even (>=2)
                            r = repOdd nr
                        in l `seq` r `seq` ZT l r
                   else let l = repOdd nl           -- right sub-tree is even, so left is odd (>=2)
                            r = repEvn nr
                        in l `seq` r `seq` ZT l r
-- n is an exact power of 2 (or 0), I.E. 0,1,2,4,8,16..
repP2 :: Int -> Tmp
repP2 0       = ET
repP2 1       = ZT ET ET
repP2 n       = let nl = n `shiftR` 1 -- nl is also an exact power of 2
                    nr = nl - 1       -- nr is one less that an exact power of 2
                    l  = repP2 nl
                    r  = repP2M1 nr
                in  l `seq` r `seq` PT l r -- BF=+1
-- n is one less than an exact power of 2, I.E. 0,1,3,7,15..
repP2M1 :: Int -> Tmp
repP2M1 0     = ET
repP2M1 n     = let sub = repP2M1 (n `shiftR` 1) in sub `seq` ZT sub sub


-- Substitute template values for real values taken from the IAList. This is for internal use only.
-- Length of IAList should match Template size
subst :: Tmp -> IAList a -> (# IntMap a, IAList a #)
subst  ET      as = (# E,as #)
subst (NT l r) as = subst_ N l r as
subst (ZT l r) as = subst_ Z l r as
subst (PT l r) as = subst_ P l r as
subst_ :: (IntKey -> IntMap a -> a -> IntMap a  -> IntMap a) -> Tmp -> Tmp -> IAList a -> (# IntMap a, IAList a #)
{-# INLINE subst_ #-}
subst_ c l r as = case subst l as of
                  (# l_,as_ #) -> case as_ of
                                  Cons ka a as__ -> case subst r as__ of
                                                    (# r_,as___ #) -> let t = c ka l_ a r_
                                                                      in t `seq` (# t,as___ #)
                                  Empt    -> error (mErr ++ "subst: List too short.")

-- | See 'Map' class method 'difference'.
differenceIntMap :: IntMap a -> IntMap b -> IntMap a
differenceIntMap ta0 tb0 = d0 ta0 tb0 where
 d0  E            _ = E
 d0  _            E = ta0
 d0 (N _ la _ _ ) _ = dH (addHeight 2# la) -- ?? As things are, we could use relative heights here!
 d0 (Z _ la _ _ ) _ = dH (addHeight 1# la)
 d0 (P _ _  _ ra) _ = dH (addHeight 2# ra)
 dH hta0 = case d hta0 ta0 tb0 of (# t,_ #) -> t
 -- d :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --              IntMap b  ->    -- 2nd IntMap (without height)
 --      (# Int#,IntMap a #)     -- Output IntMap with height
 ------------------------------------------------
 d ha  E              _             = (# E ,ha #) -- Relative heights!!
 d ha ta              E             = (# ta,ha #)
 d ha (N ka la a ra) (N kb lb _ rb) = d_ ka ((ha)-#2#) la a ((ha)-#1#) ra kb lb rb
 d ha (N ka la a ra) (Z kb lb _ rb) = d_ ka ((ha)-#2#) la a ((ha)-#1#) ra kb lb rb
 d ha (N ka la a ra) (P kb lb _ rb) = d_ ka ((ha)-#2#) la a ((ha)-#1#) ra kb lb rb
 d ha (Z ka la a ra) (N kb lb _ rb) = d_ ka ((ha)-#1#) la a ((ha)-#1#) ra kb lb rb
 d ha (Z ka la a ra) (Z kb lb _ rb) = d_ ka ((ha)-#1#) la a ((ha)-#1#) ra kb lb rb
 d ha (Z ka la a ra) (P kb lb _ rb) = d_ ka ((ha)-#1#) la a ((ha)-#1#) ra kb lb rb
 d ha (P ka la a ra) (N kb lb _ rb) = d_ ka ((ha)-#1#) la a ((ha)-#2#) ra kb lb rb
 d ha (P ka la a ra) (Z kb lb _ rb) = d_ ka ((ha)-#1#) la a ((ha)-#2#) ra kb lb rb
 d ha (P ka la a ra) (P kb lb _ rb) = d_ ka ((ha)-#1#) la a ((ha)-#2#) ra kb lb rb
 d_ ka hla la a hra ra kb lb rb =
  case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT ->                            case fork hra ra kb of
        (# hrla,rla,hrra,rra #) -> case spliceH ka la hla a rla hrla of
         (# la_,hla_ #)         -> case d hla_ la_ lb of
          (# l,hl #)            -> case d hrra rra rb of
           (# r,hr #)           -> joinH l hl r hr
  -- ka = kb
  EQ ->                case d hra ra rb of -- right
        (# r,hr #)  -> case d hla la lb of -- left
         (# l,hl #) -> joinH l hl r hr
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT ->                            case fork hla la kb of
        (# hlla,lla,hlra,lra #) -> case spliceH ka lra hlra a ra hra of
         (# ra_,hra_ #)         -> case d hra_ ra_ rb of
          (# r,hr #)            -> case d hlla lla lb of
           (# l,hl #)           -> joinH l hl r hr
 -- fork :: Int# -> IntMap a -> IntKey -> (# Int#, IntMap a, Int#, IntMap a #)
 fork hta ta kb = fork_ hta ta where
  fork_ h  E          = (# h,E,h,E #) -- Relative heights!!
  fork_ h (N k l a r) = fork__ k ((h)-#2#) l a ((h)-#1#) r
  fork_ h (Z k l a r) = fork__ k ((h)-#1#) l a ((h)-#1#) r
  fork_ h (P k l a r) = fork__ k ((h)-#1#) l a ((h)-#2#) r
  fork__ k hl l a hr r = case compareInt# k kb of
                         LT ->                        case fork_ hr r of
                               (# hx0,x0,hx1,x1 #) -> case spliceH k l hl a x0 hx0 of
                                (# x0_,hx0_ #)     -> (# hx0_,x0_,hx1,x1 #)
                         EQ -> (# hl,l,hr,r #)  -- (k,a) is dropped.
                         GT ->                        case fork_ hl l of
                               (# hx0,x0,hx1,x1 #) -> case spliceH k x1 hx1 a r hr of
                                (# x1_,hx1_ #)     -> (# hx0,x0,hx1_,x1_ #)
-----------------------------------------------------------------------
--------------------- differenceIntMap Ends Here -----------------------
-----------------------------------------------------------------------

-- | See 'Map' class method 'differenceMaybe'.
differenceMaybeIntMap :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceMaybeIntMap f ta0 tb0 = d0 ta0 tb0 where
 d0  E            _ = E
 d0  _            E = ta0
 d0 (N _ la _ _ ) _ = dH (addHeight 2# la) -- ?? As things are, we could use relative heights here!
 d0 (Z _ la _ _ ) _ = dH (addHeight 1# la)
 d0 (P _ _  _ ra) _ = dH (addHeight 2# ra)
 dH hta0 = case d hta0 ta0 tb0 of (# t,_ #) -> t
 -- d :: Int# -> IntMap a  ->    -- 1st IntMap with height
 --              IntMap b  ->    -- 2nd IntMap (without height)
 --      (# Int#,IntMap a #)     -- Output IntMap with height
 ------------------------------------------------
 d ha  E              _             = (# E ,ha #) -- Relative heights!!
 d ha ta              E             = (# ta,ha #)
 d ha (N ka la a ra) (N kb lb b rb) = d_ ka ((ha)-#2#) la a ((ha)-#1#) ra kb lb b rb
 d ha (N ka la a ra) (Z kb lb b rb) = d_ ka ((ha)-#2#) la a ((ha)-#1#) ra kb lb b rb
 d ha (N ka la a ra) (P kb lb b rb) = d_ ka ((ha)-#2#) la a ((ha)-#1#) ra kb lb b rb
 d ha (Z ka la a ra) (N kb lb b rb) = d_ ka ((ha)-#1#) la a ((ha)-#1#) ra kb lb b rb
 d ha (Z ka la a ra) (Z kb lb b rb) = d_ ka ((ha)-#1#) la a ((ha)-#1#) ra kb lb b rb
 d ha (Z ka la a ra) (P kb lb b rb) = d_ ka ((ha)-#1#) la a ((ha)-#1#) ra kb lb b rb
 d ha (P ka la a ra) (N kb lb b rb) = d_ ka ((ha)-#1#) la a ((ha)-#2#) ra kb lb b rb
 d ha (P ka la a ra) (Z kb lb b rb) = d_ ka ((ha)-#1#) la a ((ha)-#2#) ra kb lb b rb
 d ha (P ka la a ra) (P kb lb b rb) = d_ ka ((ha)-#1#) la a ((ha)-#2#) ra kb lb b rb
 d_ ka hla la a hra ra kb lb b rb =
  case compareInt# ka kb of
  -- ka < kb, so (la < ka < kb) & (ka < kb < rb)
  LT ->                                case fork hra ra kb b of
        (# hrla,rla,mba,hrra,rra #) -> case spliceH ka la hla a rla hrla of
         (# la_,hla_ #)             -> case d hla_ la_ lb of
          (# l,hl #)                -> case d hrra rra rb of
           (# r,hr #)               -> case mba of
                                       Nothing -> joinH      l hl    r hr
                                       Just a' -> spliceH kb l hl a' r hr
  -- ka = kb
  EQ ->                case d hra ra rb of -- right
        (# r,hr #)  -> case d hla la lb of -- left
         (# l,hl #) -> case f a b of
                       Nothing -> joinH      l hl    r hr
                       Just a' -> spliceH kb l hl a' r hr
  -- kb < ka, so (lb < kb < ka) & (kb < ka < ra)
  GT ->                                case fork hla la kb b of
        (# hlla,lla,mba,hlra,lra #) -> case spliceH ka lra hlra a ra hra of
         (# ra_,hra_ #)             -> case d hra_ ra_ rb of
          (# r,hr #)                -> case d hlla lla lb of
           (# l,hl #)               -> case mba of
                                       Nothing -> joinH      l hl    r hr
                                       Just a' -> spliceH kb l hl a' r hr
 -- fork :: Int# -> IntMap a -> IntKey -> b -> (# Int#, IntMap a, Maybe a, Int#, IntMap a #)
 fork hta ta kb b = fork_ hta ta where
  fork_ h  E          = (# h,E,Nothing,h,E #) -- Relative heights!!
  fork_ h (N k l a r) = fork__ k ((h)-#2#) l a ((h)-#1#) r
  fork_ h (Z k l a r) = fork__ k ((h)-#1#) l a ((h)-#1#) r
  fork_ h (P k l a r) = fork__ k ((h)-#1#) l a ((h)-#2#) r
  fork__ k hl l a hr r = case compareInt# k kb of
                         LT ->                            case fork_ hr r of
                               (# hx0,x0,mba,hx1,x1 #) -> case spliceH k l hl a x0 hx0 of
                                (# x0_,hx0_ #)         -> (# hx0_,x0_,mba,hx1,x1 #)
                         EQ -> let mba = f a b in mba `seq` (# hl,l,mba,hr,r #)
                         GT ->                            case fork_ hl l of
                               (# hx0,x0,mba,hx1,x1 #) -> case spliceH k x1 hx1 a r hr of
                                (# x1_,hx1_ #)         -> (# hx0,x0,mba,hx1_,x1_ #)
-----------------------------------------------------------------------
------------------ differenceMaybeIntMap Ends Here ---------------------
-----------------------------------------------------------------------

-- | Join two IntMaps of known height, returning an IntMap of known height.
-- It_s OK if heights are relative (I.E. if they share same fixed offset).
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
joinH :: IntMap a -> Int# -> IntMap a -> Int# -> (# IntMap a,Int# #)
joinH l hl r hr =
 case compareInt# hl hr of
 -- hr > hl
 LT -> case l of
       E             -> (# r,hr #)
       N li ll la lr -> case popRN li ll la lr of
                        (# l_,iv,v #) -> case l_ of
                                         Z _ _ _ _ -> spliceHL iv l_ ((hl)-#1#) v r hr -- dH=-1
                                         _         -> spliceHL iv l_         hl  v r hr -- dH= 0
       Z li ll la lr -> case popRZ li ll la lr of
                        (# l_,iv,v #) -> case l_ of
                                         E         -> pushHL l r hr                     -- l had only 1 element
                                         _         -> spliceHL iv l_         hl  v r hr -- dH=0
       P li ll la lr -> case popRP li ll la lr of
                        (# l_,iv,v #) -> case l_ of
                                         Z _ _ _ _ -> spliceHL iv l_ ((hl)-#1#) v r hr -- dH=-1
                                         _         -> spliceHL iv l_         hl  v r hr -- dH= 0
 -- hr = hl
 EQ -> case l of
       E             -> (# l,hl #)              -- r must be empty too
       N li ll la lr -> case popRN li ll la lr of
                        (# l_,iv,v #) -> case l_ of
                                         Z _ _ _ _ -> spliceHL iv l_ ((hl)-#1#) v r hr -- dH=-1
                                         _         -> (# Z iv l_ v r, ((hr)+#1#) #)    -- dH= 0
       Z li ll la lr -> case popRZ li ll la lr of
                        (# l_,iv,v #) -> case l_ of
                                         E         -> pushHL l r hr                     -- l had only 1 element
                                         _         -> (# Z iv l_ v r, ((hr)+#1#) #)    -- dH= 0
       P li ll la lr -> case popRP li ll la lr of
                        (# l_,iv,v #) -> case l_ of
                                         Z _ _ _ _ -> spliceHL iv l_ ((hl)-#1#) v r hr -- dH=-1
                                         _         -> (# Z iv l_ v r, ((hr)+#1#) #)    -- dH= 0
 -- hl > hr
 GT -> case r of
       E             -> (# l,hl #)
       N ri rl ra rr -> case popLN ri rl ra rr of
                        (# iv,v,r_ #) -> case r_ of
                                         Z _ _ _ _ -> spliceHR iv l hl v r_ ((hr)-#1#) -- dH=-1
                                         _         -> spliceHR iv l hl v r_         hr  -- dH= 0
       Z ri rl ra rr -> case popLZ ri rl ra rr of
                        (# iv,v,r_ #) -> case r_ of
                                         E         -> pushHR l hl r                     -- r had only 1 element
                                         _         -> spliceHR iv l hl v r_ hr          -- dH=0
       P ri rl ra rr -> case popLP ri rl ra rr of
                        (# iv,v,r_ #) -> case r_ of
                                         Z _ _ _ _ -> spliceHR iv l hl v r_ ((hr)-#1#) -- dH=-1
                                         _         -> spliceHR iv l hl v r_         hr  -- dH= 0


-- | Splice two IntMaps of known height using the supplied bridging association pair.
-- That is, the bridging pair appears \"in the middle\" of the resulting IntMap.
-- The pairs of the first tree argument are to the left of the bridging pair and
-- the pairs of the second tree are to the right of the bridging pair.
--
-- This function does not require that the IntMap heights are absolutely correct, only that
-- the difference in supplied heights is equal to the difference in actual heights. So it_s
-- OK if the input heights both have the same unknown constant offset. (The output height
-- will also have the same constant offset in this case.)
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
spliceH :: IntKey -> IntMap a -> Int# -> a -> IntMap a -> Int# -> (# IntMap a,Int# #)
-- You_d think inlining this function would make a significant difference to many functions
-- (such as set operations), but it doesn_t. It makes them marginally slower!!
spliceH ib l hl b r hr =
 case compareInt# hl hr of
 LT -> spliceHL ib l hl b r hr
 EQ -> (# Z ib l b r, ((hl)+#1#) #)
 GT -> spliceHR ib l hl b r hr

-----------------------------------------------------------------------
----------------------------- spliceHL --------------------------------
-----------------------------------------------------------------------
-- Splice tree s into the left edge of tree t (where ht>hs) using the supplied bridging pair (ib,b),
-- returning another tree of known relative height.
spliceHL :: IntKey -> IntMap a -> Int# -> a -> IntMap a -> Int# -> (# IntMap a,Int# #)
spliceHL ib s hs b t ht = let d = ((ht)-#(hs))
                          in if d ==# 1# then (# N ib s b t, ((ht)+#1#) #)
                                           else sHL ht d t
 where -- s, ib and b are free

 -- Splice two trees of known relative height where hr>hl+1, using the supplied bridging element,
 -- returning another tree of known relative height. d >= 2
 {-# INLINE sHL #-}
 sHL _  _  E              = error "spliceHL_: Bug0"          -- impossible if hr>hl
 sHL hr d (N ri rl ra rr) = let r_ = sLN ((d)-#2#) ri rl ra rr
                            in  r_ `seq` (# r_,hr #)
 sHL hr d (Z ri rl ra rr) = let r_ = sLZ ((d)-#1#) ri rl ra rr
                            in case r_ of
                               E         -> error "spliceHL: Bug1"
                               Z _ _ _ _ -> (# r_,        hr  #)
                               _         -> (# r_,((hr)+#1#) #)
 sHL hr d (P ri rl ra rr) = let r_ = sLP ((d)-#1#) ri rl ra rr
                            in  r_ `seq` (# r_,hr #)

 -- Splice into left subtree of (N i l a r), height cannot change as a result of this
 sLN 0# i  l              a r = Z i (Z ib s b l) a r                                       -- dH=0
 sLN 1# i  l              a r = Z i (N ib s b l) a r                                       -- dH=0
 sLN d    i (N li ll la lr) a r = let l_ = sLN ((d)-#2#) li ll la lr in l_ `seq` N i l_ a r
 sLN d    i (Z li ll la lr) a r = let l_ = sLZ ((d)-#1#) li ll la lr
                                  in case l_ of
                                     Z _ _ _ _ -> N i l_ a r                                 -- dH=0
                                     P _ _ _ _ -> Z i l_ a r                                 -- dH=0
                                     _         -> error "spliceHL: Bug2"                     -- impossible
 sLN d    i (P li ll la lr) a r = let l_ = sLP ((d)-#1#) li ll la lr in l_ `seq` N i l_ a r
 sLN _    _  E              _ _ = error "spliceHL: Bug3"                                     -- impossible

 -- Splice into left subtree of (Z i l a r), Z->P if dH=1, Z->Z if dH=0
 sLZ 1# i  l              a r = P i (N ib s b l) a r                                       -- Z->P, dH=1
 sLZ d    i (N li ll la lr) a r = let l_ = sLN ((d)-#2#) li ll la lr in l_ `seq` Z i l_ a r -- Z->Z, dH=0
 sLZ d    i (Z li ll la lr) a r = let l_ = sLZ ((d)-#1#) li ll la lr
                                  in case l_ of
                                     Z _ _ _ _ -> Z i l_ a r                                 -- Z->Z, dH=0
                                     P _ _ _ _ -> P i l_ a r                                 -- Z->P, dH=1
                                     _         -> error "spliceHL: Bug4"                     -- impossible
 sLZ d    i (P li ll la lr) a r = let l_ = sLP ((d)-#1#) li ll la lr in l_ `seq` Z i l_ a r -- Z->Z, dH=0
 sLZ _    _  E              _ _ = error "spliceHL: Bug5"                                     -- impossible

 -- Splice into left subtree of (P i l a r), height cannot change as a result of this
 sLP 1# i (N li ll la lr) a r = Z li (P ib s b ll) la (Z i lr a r)                         -- dH=0
 sLP 1# i (Z li ll la lr) a r = Z li (Z ib s b ll) la (Z i lr a r)                         -- dH=0
 sLP 1# i (P li ll la lr) a r = Z li (Z ib s b ll) la (N i lr a r)                         -- dH=0
 sLP d    i (N li ll la lr) a r = let l_ = sLN ((d)-#2#) li ll la lr in l_ `seq` P i l_ a r -- dH=0
 sLP d    i (Z li ll la lr) a r = sLPZ ((d)-#1#) i li ll la lr a r                          -- dH=0
 sLP d    i (P li ll la lr) a r = let l_ = sLP ((d)-#1#) li ll la lr in l_ `seq` P i l_ a r -- dH=0
 sLP _    _  E              _ _ = error "spliceHL: Bug6"

 -- Splice into left subtree of (P i (Z li ll la lr) a r)
 {-# INLINE sLPZ #-}
 sLPZ 1# i li ll                  la lr a r = Z li (N ib s b ll) la (Z i lr a r)         -- dH=0
 sLPZ d    i li (N lli lll lle llr) la lr a r = let ll_ = sLN ((d)-#2#) lli lll lle llr   -- dH=0
                                                in  ll_ `seq` P i (Z li ll_ la lr) a r
 sLPZ d    i li (Z lli lll lle llr) la lr a r = let ll_ = sLZ ((d)-#1#) lli lll lle llr   -- dH=0
                                                in case ll_ of
                                                   Z _ _ _ _ -> P i (Z li ll_ la lr) a r   -- dH=0
                                                   P _ _ _ _ -> Z li ll_ la (Z i lr a r)   -- dH=0
                                                   _         -> error "spliceHL: Bug7"     -- impossible
 sLPZ d    i li (P lli lll lle llr) la lr a r = let ll_ = sLP ((d)-#1#) lli lll lle llr   -- dH=0
                                                in  ll_ `seq` P i (Z li ll_ la lr) a r
 sLPZ _    _ _   E                  _  _  _ _ = error "spliceHL: Bug8"
-----------------------------------------------------------------------
------------------------- spliceHL Ends Here --------------------------
-----------------------------------------------------------------------

-----------------------------------------------------------------------
----------------------------- spliceHR --------------------------------
-----------------------------------------------------------------------
-- Splice tree t into the right edge of tree s (where hs>ht) using the supplied bridging pair (ib,b),
-- returning another tree of known relative height.
spliceHR :: IntKey -> IntMap a -> Int# -> a -> IntMap a -> Int# -> (# IntMap a,Int# #)
spliceHR ib s hs b t ht = let d = ((hs)-#(ht))
                          in if d ==# 1# then (# P ib s b t, ((hs)+#1#) #)
                                           else sHR hs d s
 where -- t, ib and b are free

 {-# INLINE sHR #-}
 sHR _  _  E           = error "spliceHL: Bug0"          -- impossible if hl>hr
 sHR hl d (N li ll la lr) = let l_ = sRN ((d)-#1#) li ll la lr
                            in  l_ `seq` (# l_,hl #)
 sHR hl d (Z li ll la lr) = let l_ = sRZ ((d)-#1#) li ll la lr
                            in case l_ of
                               E         -> error "spliceHL: Bug1"
                               Z _ _ _ _ -> (# l_,        hl  #)
                               _         -> (# l_,((hl)+#1#) #)
 sHR hl d (P li ll la lr) = let l_ = sRP ((d)-#2#) li ll la lr
                            in  l_ `seq` (# l_,hl #)

 -- Splice into right subtree of (P i l a r), height cannot change as a result of this
 sRP 0# i l a  r              = Z i l a (Z ib r b t)                                       -- dH=0
 sRP 1# i l a  r              = Z i l a (P ib r b t)                                       -- dH=0
 sRP d    i l a (N ri rl ra rr) = let r_ = sRN ((d)-#1#) ri rl ra rr in r_ `seq` P i l a r_
 sRP d    i l a (Z ri rl ra rr) = let r_ = sRZ ((d)-#1#) ri rl ra rr
                                  in case r_ of
                                     Z _ _ _ _ -> P i l a r_                                 -- dH=0
                                     N _ _ _ _ -> Z i l a r_                                 -- dH=0
                                     _         -> error "spliceHL: Bug2"                     -- impossible
 sRP d    i l a (P ri rl ra rr) = let r_ = sRP ((d)-#2#) ri rl ra rr in r_ `seq` P i l a r_
 sRP _    _ _ _  E              = error "spliceHL: Bug3"                                     -- impossible

 -- Splice into right subtree of (Z i l a r), Z->N if dH=1, Z->Z if dH=0
 sRZ 1# i l a  r           = N i l a (P ib r b t)                                          -- Z->N, dH=1
 sRZ d    i l a (N ri rl ra rr) = let r_ = sRN ((d)-#1#) ri rl ra rr in r_ `seq` Z i l a r_ -- Z->Z, dH=0
 sRZ d    i l a (Z ri rl ra rr) = let r_ = sRZ ((d)-#1#) ri rl ra rr
                                  in case r_ of
                                     Z _ _ _ _ -> Z i l a r_                                 -- Z->Z, dH=0
                                     N _ _ _ _ -> N i l a r_                                 -- Z->N, dH=1
                                     _         -> error "spliceHL: Bug4"                     -- impossible
 sRZ d    i l a (P ri rl ra rr) = let r_ = sRP ((d)-#2#) ri rl ra rr in r_ `seq` Z i l a r_ -- Z->Z, dH=0
 sRZ _    _ _ _  E              = error "spliceHL: Bug5"                                     -- impossible

 -- Splice into right subtree of (N i l a r), height cannot change as a result of this
 sRN 1# i l a (N ri rl ra rr) = Z ri (P i l a rl) ra (Z ib rr b t)                         -- dH=0
 sRN 1# i l a (Z ri rl ra rr) = Z ri (Z i l a rl) ra (Z ib rr b t)                         -- dH=0
 sRN 1# i l a (P ri rl ra rr) = Z ri (Z i l a rl) ra (N ib rr b t)                         -- dH=0
 sRN d    i l a (N ri rl ra rr) = let r_ = sRN ((d)-#1#) ri rl ra rr in r_ `seq` N i l a r_ -- dH=0
 sRN d    i l a (Z ri rl ra rr) = sRNZ ((d)-#1#) i l a ri rl ra rr                          -- dH=0
 sRN d    i l a (P ri rl ra rr) = let r_ = sRP ((d)-#2#) ri rl ra rr in r_ `seq` N i l a r_ -- dH=0
 sRN _    _ _ _  E              = error "spliceHL: Bug6"

 -- Splice into right subtree of (N i l a (Z ri rl ra rr))
 {-# INLINE sRNZ #-}
 sRNZ 1# i l a ri rl ra rr                  = Z ri (Z i l a rl) ra (P ib rr b t)           -- dH=0
 sRNZ d    i l a ri rl ra (N rri rrl rre rrr) = let rr_ = sRN ((d)-#1#) rri rrl rre rrr
                                                in  rr_ `seq` N i l a (Z ri rl ra rr_)       -- dH=0
 sRNZ d    i l a ri rl ra (Z rri rrl rre rrr) = let rr_ = sRZ ((d)-#1#) rri rrl rre rrr     -- dH=0
                                                in case rr_ of
                                                   Z _ _ _ _ -> N i l a (Z ri rl ra rr_)     -- dH=0
                                                   N _ _ _ _ -> Z ri (Z i l a rl) ra rr_     -- dH=0
                                                   _         -> error "spliceHL: Bug7"       -- impossible
 sRNZ d    i l a ri rl ra (P rri rrl rre rrr) = let rr_ = sRP ((d)-#2#) rri rrl rre rrr     -- dH=0
                                                in rr_ `seq` N i l a (Z ri rl ra rr_)
 sRNZ _    _ _ _ _  _  _   E                  = error "spliceHL: Bug8"
-----------------------------------------------------------------------
------------------------- spliceHR Ends Here --------------------------
-----------------------------------------------------------------------


-- | Push a singleton IntMap to the leftmost position of an IntMap of known height.
-- Returns an IntMap of known height.
-- It_s OK if height is relative, with fixed offset. In this case the height of the result
-- will have the same fixed offset.
pushHL :: IntMap a -> IntMap a -> Int# -> (# IntMap a,Int# #)
pushHL t0 t h = case t of
                E         -> (# t0, ((h)+#1#) #) -- Relative Heights
                N i l a r -> let t_ = potNL i l a r in t_ `seq` (# t_,h #)
                P i l a r -> let t_ = potPL i l a r in t_ `seq` (# t_,h #)
                Z i l a r -> let t_ = potZL i l a r
                             in case t_ of
                                Z _ _ _ _ -> (# t_,         h  #)
                                P _ _ _ _ -> (# t_, ((h)+#1#) #)
                                _         -> error "pushHL: Bug0" -- impossible
 where
 ----------------------------- LEVEL 2 ---------------------------------
 --                      potNL, potZL, potPL                          --
 -----------------------------------------------------------------------

 -- (potNL i l a r): Put t0 in L subtree of (N i l a r), BF=-1 (Never requires rebalancing) , (never returns P)
 potNL i  E              a r = Z i t0 a r                        -- L subtree empty, H:0->1, parent BF:-1-> 0
 potNL i (N li ll la lr) a r = let l_ = potNL li ll la lr        -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                               in l_ `seq` N i l_ a r
 potNL i (P li ll la lr) a r = let l_ = potPL li ll la lr        -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                               in l_ `seq` N i l_ a r
 potNL i (Z li ll la lr) a r = let l_ = potZL li ll la lr        -- L subtree BF= 0, so need to look for changes
                               in case l_ of
                               Z _ _ _ _ -> N i l_ a r           -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                               P _ _ _ _ -> Z i l_ a r           -- L subtree BF:0->+1, H:h->h+1, parent BF:-1-> 0
                               _         -> error "pushHL: Bug1" -- impossible

 -- (potZL i l a r): Put t0 in L subtree of (Z i l a r), BF= 0  (Never requires rebalancing) , (never returns N)
 potZL i  E              a r = P i t0 a r                        -- L subtree        H:0->1, parent BF: 0->+1
 potZL i (N li ll la lr) a r = let l_ = potNL li ll la lr        -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                               in l_ `seq` Z i l_ a r
 potZL i (P li ll la lr) a r = let l_ = potPL li ll la lr        -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                               in l_ `seq` Z i l_ a r
 potZL i (Z li ll la lr) a r = let l_ = potZL li ll la lr        -- L subtree BF= 0, so need to look for changes
                               in case l_ of
                               Z _ _ _ _ -> Z i l_ a r           -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                               N _ _ _ _ -> error "pushHL: Bug2" -- impossible
                               _         -> P i l_ a r           -- L subtree BF: 0->+1, H:h->h+1, parent BF: 0->+1

      -------- This case (PL) may need rebalancing if it goes to LEVEL 3 ---------

 -- (potPL i l a r): Put t0 in L subtree of (P i l a r), BF=+1 , (never returns N)
 potPL _  E              _ _ = error "pushHL: Bug3"       -- impossible if BF=+1
 potPL i (N li ll la lr) a r = let l_ = potNL li ll la lr -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                               in l_ `seq` P i l_ a r
 potPL i (P li ll la lr) a r = let l_ = potPL li ll la lr -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                               in l_ `seq` P i l_ a r
 potPL i (Z li ll la lr) a r = potPLL i li ll la lr a r   -- LL (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            potPLL                                 --
 -----------------------------------------------------------------------

 -- (potPLL i li ll la lr a r): Put t0 in LL subtree of (P i (Z li ll la lr) a r) , (never returns N)
 {-# INLINE potPLL #-}
 potPLL i li  E                  la lr a r = Z li t0 la (Z i lr a r) -- r and lr must also be E, special CASE LL!!
 potPLL i li (N lli lll lla llr) la lr a r = let ll_ = potNL lli lll lla llr          -- LL subtree BF<>0, H:h->h, so no change
                                             in ll_ `seq` P i (Z li ll_ la lr) a r
 potPLL i li (P lli lll lla llr) la lr a r = let ll_ = potPL lli lll lla llr          -- LL subtree BF<>0, H:h->h, so no change
                                             in ll_ `seq` P i (Z li ll_ la lr) a r
 potPLL i li (Z lli lll lla llr) la lr a r = let ll_ = potZL lli lll lla llr          -- LL subtree BF= 0, so need to look for changes
                                            in case ll_ of
                                                Z _ _ _ _ -> P i (Z li ll_ la lr) a r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                                N _ _ _ _ -> error "pushHL: Bug4"     -- impossible
                                                _         -> Z li ll_ la (Z i lr a r) -- LL subtree BF: 0->+1, H:h->h+1, parent BF:-1->-2, CASE LL !!
-----------------------------------------------------------------------
-------------------------- pushHL Ends Here ---------------------------
-----------------------------------------------------------------------


-- | Push a singleton IntMap to the rightmost position of an IntMap of known height.
-- Returns an IntMap of known height.
-- It_s OK if height is relative, with fixed offset. In this case the height of the result
-- will have the same fixed offset.
pushHR :: IntMap a -> Int# -> IntMap a -> (# IntMap a,Int# #)
pushHR t h t0 = case t of
                E         -> (# t0, ((h)+#1#) #) -- Relative Heights
                N i l a r -> let t_ = potNR i l a r in t_ `seq` (# t_,h #)
                P i l a r -> let t_ = potPR i l a r in t_ `seq` (# t_,h #)
                Z i l a r -> let t_ = potZR i l a r
                             in case t_ of
                                Z _ _ _ _ -> (# t_,         h  #)
                                N _ _ _ _ -> (# t_, ((h)+#1#) #)
                                _         -> error "pushHR: Bug0" -- impossible
 where
 ----------------------------- LEVEL 2 ---------------------------------
 --                      potNR, potZR, potPR                          --
 -----------------------------------------------------------------------

 -- (potZR i l a r): Put t0 in R subtree of (Z i l a r), BF= 0 (Never requires rebalancing) , (never returns P)
 potZR i l a  E              = N i l a t0                       -- R subtree        H:0->1, parent BF: 0->-1
 potZR i l a (N ri rl ra rr) = let r_ = potNR ri rl ra rr       -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                               in r_ `seq` Z i l a r_
 potZR i l a (P ri rl ra rr) = let r_ = potPR ri rl ra rr       -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                               in r_ `seq` Z i l a r_
 potZR i l a (Z ri rl ra rr) = let r_ = potZR ri rl ra rr       -- R subtree BF= 0, so need to look for changes
                               in case r_ of
                               Z _ _ _ _ -> Z i l a r_          -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                               N _ _ _ _ -> N i l a r_          -- R subtree BF: 0->-1, H:h->h+1, parent BF: 0->-1
                               _         -> error "pushHR: Bug1" -- impossible

 -- (potPR i l a r): Put t0 in R subtree of (P i l a r), BF=+1 (Never requires rebalancing) , (never returns N)
 potPR i l a  E              = Z i l a t0                       -- R subtree empty, H:0->1,     parent BF:+1-> 0
 potPR i l a (N ri rl ra rr) = let r_ = potNR ri rl ra rr       -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                               in r_ `seq` P i l a r_
 potPR i l a (P ri rl ra rr) = let r_ = potPR ri rl ra rr       -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                               in r_ `seq` P i l a r_
 potPR i l a (Z ri rl ra rr) = let r_ = potZR ri rl ra rr       -- R subtree BF= 0, so need to look for changes
                               in case r_ of
                               Z _ _ _ _ -> P i l a r_          -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                               N _ _ _ _ -> Z i l a r_          -- R subtree BF:0->-1, H:h->h+1, parent BF:+1-> 0
                               _         -> error "pushHR: Bug2" -- impossible

      -------- This case (NR) may need rebalancing if it goes to LEVEL 3 ---------

 -- (potNR i l a r): Put t0 in R subtree of (N i l a r), BF=-1 , (never returns P)
 potNR _ _ _  E              = error "pushHR: Bug3"           -- impossible if BF=-1
 potNR i l a (N ri rl ra rr) = let r_ = potNR ri rl ra rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                               in r_ `seq` N i l a r_
 potNR i l a (P ri rl ra rr) = let r_ = potPR ri rl ra rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                               in r_ `seq` N i l a r_
 potNR i l a (Z ri rl ra rr) = potNRR i l a ri rl ra rr       -- RR (never returns P)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            potNRR                                 --
 -----------------------------------------------------------------------

 -- (potNRR i l a ri rl ra rr): Put t0 in RR subtree of (N i l a (Z ri rl ra rr)) , (never returns P)
 {-# INLINE potNRR #-}
 potNRR i l a ri rl ra  E                  = Z ri (Z i l a rl) ra t0               -- l and rl must also be E, special CASE RR!!
 potNRR i l a ri rl ra (N rri rrl rra rrr) = let rr_ = potNR rri rrl rra rrr       -- RR subtree BF<>0, H:h->h, so no change
                                             in rr_ `seq` N i l a (Z ri rl ra rr_)
 potNRR i l a ri rl ra (P rri rrl rra rrr) = let rr_ = potPR rri rrl rra rrr       -- RR subtree BF<>0, H:h->h, so no change
                                             in rr_ `seq` N i l a (Z ri rl ra rr_)
 potNRR i l a ri rl ra (Z rri rrl rra rrr) = let rr_ = potZR rri rrl rra rrr       -- RR subtree BF= 0, so need to look for changes
                                             in case rr_ of
                                             Z _ _ _ _ -> N i l a (Z ri rl ra rr_) -- RR subtree BF: 0-> 0, H:h->h, so no change
                                             N _ _ _ _ -> Z ri (Z i l a rl) ra rr_ -- RR subtree BF: 0->-1, H:h->h+1, parent BF:-1->-2, CASE RR !!
                                             _         -> error "pushHR: Bug4"     -- impossible
-----------------------------------------------------------------------
-------------------------- pushHR Ends Here ---------------------------
-----------------------------------------------------------------------

-- | Delete the association pair with the supplied IntKey from an IntMap.
-- For use only if it is already known to contain an entry for the supplied key.
-- This function raises an error if there is no such pair.
del :: IntKey -> IntMap a -> IntMap a
del _   E          = error "del: IntKey not found."
del k0 (N k l a r) = delN k0 k l a r
del k0 (Z k l a r) = delZ k0 k l a r
del k0 (P k l a r) = delP k0 k l a r

-- | Same as 'del', but takes the (relative) tree height as an extra argument and
-- returns the updated (relative) tree height.
delH :: IntKey -> Int# -> IntMap a -> (# IntMap a,Int# #)
delH _  _   E          = error "delH: IntKey not found."
delH k0 ht (N k l a r) = let t_ = delN k0 k l a r in
                         case t_ of
                         Z _ _ _ _ -> (# t_,((ht)-#1#) #)
                         _         -> (# t_,        ht  #)
delH k0 ht (Z k l a r) = let t_ = delZ k0 k l a r in
                         case t_ of
                         E         -> (# t_,((ht)-#1#) #)
                         _         -> (# t_,        ht  #)
delH k0 ht (P k l a r) = let t_ = delP k0 k l a r in
                         case t_ of
                         Z _ _ _ _ -> (# t_,((ht)-#1#) #)
                         _         -> (# t_,        ht  #)

----------------------------- LEVEL 1 ---------------------------------
--                       delN, delZ, delP                            --
-----------------------------------------------------------------------

-- Delete from (N k l a r)
delN :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delN k0 k l a r = case compareInt# k0 k of
                  LT -> delNL k0 k l a r
                  EQ -> subN       l   r
                  GT -> delNR k0 k l a r

-- Delete from (Z k l a r)
delZ :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delZ k0 k l a r = case compareInt# k0 k of
                  LT -> delZL k0 k l a r
                  EQ -> subZR      l   r
                  GT -> delZR k0 k l a r

-- Delete from (P k l a r)
delP :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delP k0 k l a r = case compareInt# k0 k of
                  LT -> delPL k0 k l a r
                  EQ -> subP       l   r
                  GT -> delPR k0 k l a r

----------------------------- LEVEL 2 ---------------------------------
--                      delNL, delZL, delPL                          --
--                      delNR, delZR, delPR                          --
-----------------------------------------------------------------------

-- Delete from the left subtree of (N k l a r)
delNL :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delNL _  _  E              _ _ = error "assertDelete: IntKey not found."     -- Left sub-tree is empty
delNL k0 k (N lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> chkLN k (delNL k0 lk ll la lr) a r
                                 EQ -> chkLN k (subN        ll    lr) a r
                                 GT -> chkLN k (delNR k0 lk ll la lr) a r
delNL k0 k (Z lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> let l_ = delZL k0 lk ll la lr in l_ `seq` N k l_ a r  -- height can't change
                                 EQ -> chkLN_ k (subZR      ll    lr) a r                    -- << But it can here
                                 GT -> let l_ = delZR k0 lk ll la lr in l_ `seq` N k l_ a r  -- height can't change
delNL k0 k (P lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> chkLN k (delPL k0 lk ll la lr) a r
                                 EQ -> chkLN k (subP        ll    lr) a r
                                 GT -> chkLN k (delPR k0 lk ll la lr) a r

-- Delete from the right subtree of (N k l a r)
delNR :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delNR _  _ _ _  E              = error "delNR: Bug0"             -- Impossible
delNR k0 k l a (N rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> chkRN k l a (delNL k0 rk rl ra rr)
                                 EQ -> chkRN k l a (subN        rl    rr)
                                 GT -> chkRN k l a (delNR k0 rk rl ra rr)
delNR k0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> let r_ = delZL k0 rk rl ra rr in r_ `seq` N k l a r_   -- height can't change
                                 EQ -> chkRN_ k l a (subZL  rl    rr)                         -- << But it can here
                                 GT -> let r_ = delZR k0 rk rl ra rr in r_ `seq` N k l a r_   -- height can't change
delNR k0 k l a (P rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> chkRN k l a (delPL k0 rk rl ra rr)
                                 EQ -> chkRN k l a (subP        rl    rr)
                                 GT -> chkRN k l a (delPR k0 rk rl ra rr)

-- Delete from the left subtree of (Z k l a r)
delZL :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delZL _  _  E              _ _ = error "assertDelete: IntKey not found."  -- Left sub-tree is empty
delZL k0 k (N lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> chkLZ k (delNL k0 lk ll la lr) a r
                                 EQ -> chkLZ k (subN        ll    lr) a r
                                 GT -> chkLZ k (delNR k0 lk ll la lr) a r
delZL k0 k (Z lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> let l_ = delZL k0 lk ll la lr in l_ `seq` Z k l_ a r  -- height can't change
                                 EQ -> chkLZ_ k (subZR      ll    lr) a r                    -- << But it can here
                                 GT -> let l_ = delZR k0 lk ll la lr in l_ `seq` Z k l_ a r  -- height can't change
delZL k0 k (P lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> chkLZ k (delPL k0 lk ll la lr) a r
                                 EQ -> chkLZ k (subP        ll    lr) a r
                                 GT -> chkLZ k (delPR k0 lk ll la lr) a r

-- Delete from the right subtree of (Z k l a r)
delZR :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delZR _  _ _ _  E              = error "assertDelete: IntKey not found."      -- Right sub-tree is empty
delZR k0 k l a (N rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> chkRZ k l a (delNL k0 rk rl ra rr)
                                 EQ -> chkRZ k l a (subN        rl    rr)
                                 GT -> chkRZ k l a (delNR k0 rk rl ra rr)
delZR k0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> let r_ = delZL k0 rk rl ra rr in r_ `seq` Z k l a r_  -- height can't change
                                 EQ -> chkRZ_ k l a (subZL  rl    rr)                        -- << But it can here
                                 GT -> let r_ = delZR k0 rk rl ra rr in r_ `seq` Z k l a r_  -- height can't change
delZR k0 k l a (P rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> chkRZ k l a (delPL k0 rk rl ra rr)
                                 EQ -> chkRZ k l a (subP        rl    rr)
                                 GT -> chkRZ k l a (delPR k0 rk rl ra rr)

-- Delete from the left subtree of (P k l a r)
delPL :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delPL _  _  E              _ _ = error "delPL: Bug0"             -- Impossible
delPL k0 k (N lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> chkLP k (delNL k0 lk ll la lr) a r
                                 EQ -> chkLP k (subN        ll    lr) a r
                                 GT -> chkLP k (delNR k0 lk ll la lr) a r
delPL k0 k (Z lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> let l_ = delZL k0 lk ll la lr in l_ `seq` P k l_ a r  -- height can't change
                                 EQ -> chkLP_ k (subZR      ll    lr) a r                    -- << But it can here
                                 GT -> let l_ = delZR k0 lk ll la lr in l_ `seq` P k l_ a r  -- height can't change
delPL k0 k (P lk ll la lr) a r = case compareInt# k0 lk of
                                 LT -> chkLP k (delPL k0 lk ll la lr) a r
                                 EQ -> chkLP k (subP        ll    lr) a r
                                 GT -> chkLP k (delPR k0 lk ll la lr) a r

-- Delete from the right subtree of (P l a r)
delPR :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> IntMap a
delPR _  _ _ _  E              = error "assertDelete: IntKey not found."       -- Right sub-tree is empty
delPR k0 k l a (N rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> chkRP k l a (delNL k0 rk rl ra rr)
                                 EQ -> chkRP k l a (subN        rl    rr)
                                 GT -> chkRP k l a (delNR k0 rk rl ra rr)
delPR k0 k l a (Z rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> let r_ = delZL k0 rk rl ra rr in r_ `seq` P k l a r_  -- height can't change
                                 EQ -> chkRP_ k l a (subZL  rl    rr)                        -- << But it can here
                                 GT -> let r_ = delZR k0 rk rl ra rr in r_ `seq` P k l a r_  -- height can't change
delPR k0 k l a (P rk rl ra rr) = case compareInt# k0 rk of
                                 LT -> chkRP k l a (delPL k0 rk rl ra rr)
                                 EQ -> chkRP k l a (subP        rl    rr)
                                 GT -> chkRP k l a (delPR k0 rk rl ra rr)
-----------------------------------------------------------------------
------------------------- del/delH End Here ---------------------------
-----------------------------------------------------------------------


-----------------------------------------------------------------------
------------------------ popL Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- popL LEVEL 1 -------------------------------
--                      popLN, popLZ, popLP                          --
-----------------------------------------------------------------------
-- Delete leftmost from (N k l a r)
popLN :: IntKey -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
popLN k  E              a r = (# k,a,r #)                  -- Terminal case, r must be of form (Z a ra E)
popLN k (N lk ll la lr) a r = case popLN lk ll la lr of
                              (# iv,v,l #) -> let t = chkLN k l a r in  t `seq` (# iv,v,t #)
popLN k (Z lk ll la lr) a r = popLNZ k lk ll la lr a r
popLN k (P lk ll la lr) a r = case popLP lk ll la lr of
                              (# iv,v,l #) -> let t = chkLN k l a r in  t `seq` (# iv,v,t #)

-- Delete leftmost from (Z k l a r)
popLZ :: IntKey -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
popLZ k  E              a _ = (# k,a,E #)                  -- Terminal case, r must be E
popLZ k (N lk ll la lr) a r = popLZN k lk ll la lr a r
popLZ k (Z lk ll la lr) a r = popLZZ k lk ll la lr a r
popLZ k (P lk ll la lr) a r = popLZP k lk ll la lr a r

-- Delete leftmost from (P k l a r)
popLP :: IntKey -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
popLP _  E              _ _ = error "popLP: Bug!"        -- Impossible if BF=+1
popLP k (N lk ll la lr) a r = case popLN lk ll la lr of
                              (# iv,v,l #) -> let t = chkLP k l a r in  t `seq` (# iv,v,t #)
popLP k (Z lk ll la lr) a r = popLPZ k lk ll la lr a r
popLP k (P lk ll la lr) a r = case popLP lk ll la lr of
                              (# iv,v,l #) -> let t = chkLP k l a r in  t `seq` (# iv,v,t #)

-------------------------- popL LEVEL 2 -------------------------------
--                     popLNZ, popLZZ, popLPZ                        --
--                        popLZN, popLZP                             --
-----------------------------------------------------------------------

-- Delete leftmost from (N k (Z lk ll la lr) a r), height of left sub-tree can't change in this case
popLNZ :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
{-# INLINE popLNZ #-}
popLNZ k lk  E                  la _  a r = let t = rebalN k E a r        -- Terminal case, Needs rebalancing
                                            in  t `seq` (# lk,la,t #)
popLNZ k lk (N llk lll lla llr) la lr a r = case popLZN lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,N k l a r #)
popLNZ k lk (Z llk lll lla llr) la lr a r = case popLZZ lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,N k l a r #)
popLNZ k lk (P llk lll lla llr) la lr a r = case popLZP lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,N k l a r #)

-- Delete leftmost from (Z k (Z lk ll la lr) a r), height of left sub-tree can't change in this case
-- Don't INLINE this!
popLZZ :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
popLZZ k lk  E                  la _  a r = (# lk,la,N k E a r #)                     -- Terminal case
popLZZ k lk (N llk lll lla llr) la lr a r = case popLZN lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,Z k l a r #)
popLZZ k lk (Z llk lll lla llr) la lr a r = case popLZZ lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,Z k l a r #)
popLZZ k lk (P llk lll lla llr) la lr a r = case popLZP lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,Z k l a r #)

-- Delete leftmost from (P k (Z lk ll la lr) a r), height of left sub-tree can't change in this case
popLPZ :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
{-# INLINE popLPZ #-}
popLPZ k lk  E                  la _  a _ = (# lk,la,Z k E a E #)                     -- Terminal case
popLPZ k lk (N llk lll lla llr) la lr a r = case popLZN lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,P k l a r #)
popLPZ k lk (Z llk lll lla llr) la lr a r = case popLZZ lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,P k l a r #)
popLPZ k lk (P llk lll lla llr) la lr a r = case popLZP lk llk lll lla llr la lr of
                                            (# iv,v,l #) -> (# iv,v,P k l a r #)

-- Delete leftmost from (Z k (N lk ll la lr) a r)
-- Don't INLINE this!
popLZN :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
popLZN k lk ll la lr a r = case popLN lk ll la lr of
                           (# iv,v,l #) -> let t = chkLZ k l a r in  t `seq` (# iv,v,t #)
-- Delete leftmost from (Z k (P lk ll la lr) a r)
-- Don't INLINE this!
popLZP :: IntKey -> IntKey -> IntMap a -> a -> IntMap a -> a -> IntMap a -> (# IntKey,a,IntMap a #)
popLZP k lk ll la lr a r = case popLP lk ll la lr of
                           (# iv,v,l #) -> let t = chkLZ k l a r in t `seq` (# iv,v,t #)
-----------------------------------------------------------------------
-------------------------- popL Ends Here -----------------------------
-----------------------------------------------------------------------



-----------------------------------------------------------------------
------------------------ popR Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- popR LEVEL 1 -------------------------------
--                      popRN, popRZ, popRP                          --
-----------------------------------------------------------------------
-- Delete rightmost from (N k l a r)
popRN :: IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
popRN _ _ _  E              = error "popRN: Bug!"        -- Impossible if BF=-1
popRN k l a (N rk rl ra rr) = case popRN rk rl ra rr of
                              (# r,iv,v #) -> let t = chkRN k l a r in t `seq` (# t,iv,v #)
popRN k l a (Z rk rl ra rr) = popRNZ k l a rk rl ra rr
popRN k l a (P rk rl ra rr) = case popRP rk rl ra rr of
                              (# r,iv,v #) -> let t = chkRN k l a r in t `seq` (# t,iv,v #)

-- Delete rightmost from (Z k l a r)
popRZ :: IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
popRZ k _ a  E              = (# E,k,a #)     -- Terminal case, l must be E
popRZ k l a (N rk rl ra rr) = popRZN k l a rk rl ra rr
popRZ k l a (Z rk rl ra rr) = popRZZ k l a rk rl ra rr
popRZ k l a (P rk rl ra rr) = popRZP k l a rk rl ra rr

-- Delete rightmost from (P k l a r)
popRP :: IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
popRP k l a  E              = (# l,k,a #)      -- Terminal case, l must be of form (Z a la E)
popRP k l a (N rk rl ra rr) = case popRN rk rl ra rr of
                              (# r,iv,v #) -> let t = chkRP k l a r in t `seq` (# t,iv,v #)
popRP k l a (Z rk rl ra rr) = popRPZ k l a rk rl ra rr
popRP k l a (P rk rl ra rr) = case popRP rk rl ra rr of
                              (# r,iv,v #) -> let t = chkRP k l a r in t `seq` (# t,iv,v #)

-------------------------- popR LEVEL 2 -------------------------------
--                     popRNZ, popRZZ, popRPZ                        --
--                        popRZN, popRZP                             --
-----------------------------------------------------------------------

-- Delete rightmost from (N k l a (Z rk rl ra rr)), height of right sub-tree can't change in this case
popRNZ :: IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
{-# INLINE popRNZ #-}
popRNZ k _ a rk _  ra  E                  = (# Z k E a E,rk,ra #)    -- Terminal case
popRNZ k l a rk rl ra (N rrk rrl rra rrr) = case popRZN rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# N k l a r,iv,v #)
popRNZ k l a rk rl ra (Z rrk rrl rra rrr) = case popRZZ rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# N k l a r,iv,v #)
popRNZ k l a rk rl ra (P rrk rrl rra rrr) = case popRZP rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# N k l a r,iv,v #)

-- Delete rightmost from (Z k l a (Z rk rl ra rr)), height of right sub-tree can't change in this case
-- Don't INLINE this!
popRZZ :: IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
popRZZ k l a rk _  ra  E                  = (# P k l a E,rk,ra #)  -- Terminal case
popRZZ k l a rk rl ra (N rrk rrl rra rrr) = case popRZN rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# Z k l a r,iv,v #)
popRZZ k l a rk rl ra (Z rrk rrl rra rrr) = case popRZZ rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# Z k l a r,iv,v #)
popRZZ k l a rk rl ra (P rrk rrl rra rrr) = case popRZP rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# Z k l a r,iv,v #)

-- Delete rightmost from (P k l a (Z rk rl ra rr)), height of right sub-tree can't change in this case
popRPZ :: IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
{-# INLINE popRPZ #-}
popRPZ k l a rk _  ra  E                  = let t = rebalP k l a E    -- Terminal case, Needs rebalancing
                                            in  t `seq` (# t,rk,ra #)
popRPZ k l a rk rl ra (N rrk rrl rra rrr) = case popRZN rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# P k l a r,iv,v #)
popRPZ k l a rk rl ra (Z rrk rrl rra rrr) = case popRZZ rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# P k l a r,iv,v #)
popRPZ k l a rk rl ra (P rrk rrl rra rrr) = case popRZP rk rl ra rrk rrl rra rrr of
                                            (# r,iv,v #) -> (# P k l a r,iv,v #)

-- Delete rightmost from (Z k l a (N rk rl ra rr))
-- Don't INLINE this!
popRZN :: IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
popRZN k l a rk rl ra rr = case popRN rk rl ra rr of
                           (# r,iv,v #) -> let t = chkRZ k l a r in  t `seq` (# t,iv,v #)

-- Delete rightmost from (Z k l a (P rk rl ra rr))
-- Don't INLINE this!
popRZP :: IntKey -> IntMap a -> a -> IntKey -> IntMap a -> a -> IntMap a -> (# IntMap a, IntKey, a #)
popRZP k l a rk rl ra rr = case popRP rk rl ra rr of
                           (# r,iv,v #) -> let t = chkRZ k l a r in  t `seq` (# t,iv,v #)
-----------------------------------------------------------------------
-------------------------- popR Ends Here -----------------------------
-----------------------------------------------------------------------



{-************************** Balancing Utilities Below Here ************************************-}

-- Rebalance a tree of form (N k l a r) which has become unbalanced as
-- a result of the height of the left sub-tree (l) decreasing by 1.
-- N.B Result is never of form (N _ _ _ _) (or E!)
rebalN :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
rebalN _ _ _  E                               = error "rebalN: Bug0"                     -- impossible case
rebalN k l a (N rk rl                  ra rr) = Z rk (Z k l a rl) ra rr                  -- N->Z, dH=-1
rebalN k l a (Z rk rl                  ra rr) = P rk (N k l a rl) ra rr                  -- N->P, dH= 0
rebalN _ _ _ (P _   E                  _  _ ) = error "rebalN: Bug1"                     -- impossible case
rebalN k l a (P rk (N rlk rll rla rlr) ra rr) = Z rlk (P k l a rll) rla (Z rk rlr ra rr) -- N->Z, dH=-1
rebalN k l a (P rk (Z rlk rll rla rlr) ra rr) = Z rlk (Z k l a rll) rla (Z rk rlr ra rr) -- N->Z, dH=-1
rebalN k l a (P rk (P rlk rll rla rlr) ra rr) = Z rlk (Z k l a rll) rla (N rk rlr ra rr) -- N->Z, dH=-1

-- Rebalance a tree of form (P k l a r) which has become unbalanced as
-- a result of the height of the right sub-tree (r) decreasing by 1.
-- N.B Result is never of form (P _ _ _ _) (or E!)
rebalP :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
rebalP _  E                               _ _ = error "rebalP: Bug0"                     -- impossible case
rebalP k (P lk ll la lr                 ) a r = Z lk ll la (Z k lr a r)                  -- P->Z, dH=-1
rebalP k (Z lk ll la lr                 ) a r = N lk ll la (P k lr a r)                  -- P->N, dH= 0
rebalP _ (N _  _  _   E                 ) _ _ = error "rebalP: Bug1"                     -- impossible case
rebalP k (N lk ll la (P lrk lrl lra lrr)) a r = Z lrk (Z lk ll la lrl) lra (N k lrr a r) -- P->Z, dH=-1
rebalP k (N lk ll la (Z lrk lrl lra lrr)) a r = Z lrk (Z lk ll la lrl) lra (Z k lrr a r) -- P->Z, dH=-1
rebalP k (N lk ll la (N lrk lrl lra lrr)) a r = Z lrk (P lk ll la lrl) lra (Z k lrr a r) -- P->Z, dH=-1

-- Check for height changes in left subtree of (N k l a r),
-- where l was (N lk ll la lr) or (P lk ll la lr)
chkLN :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkLN k l a r = case l of
                E         -> error "chkLN: Bug0"     -- impossible if BF<>0
                N _ _ _ _ -> N k l a r               -- BF +/-1 -> -1, so dH= 0
                Z _ _ _ _ -> rebalN k l a r          -- BF +/-1 ->  0, so dH=-1
                P _ _ _ _ -> N k l a r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in left subtree of (Z k l a r),
-- where l was (N lk ll la lr) or (P lk ll la lr)
chkLZ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkLZ k l a r = case l of
                E         -> error "chkLZ: Bug0"   -- impossible if BF<>0
                N _ _ _ _ -> Z k l a r             -- BF +/-1 -> -1, so dH= 0
                Z _ _ _ _ -> N k l a r             -- BF +/-1 ->  0, so dH=-1
                P _ _ _ _ -> Z k l a r             -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in left subtree of (P k l a r),
-- where l was (N lk ll la lr) or (P lk ll la lr)
chkLP :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkLP k l a r = case l of
                E         -> error "chkLP: Bug0"   -- impossible if BF<>0
                N _ _ _ _ -> P k l a r             -- BF +/-1 -> -1, so dH= 0
                Z _ _ _ _ -> Z k l a r             -- BF +/-1 ->  0, so dH=-1
                P _ _ _ _ -> P k l a r             -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (N k l a r),
-- where r was (N rk rl ra rr) or (P rk rl ra rr)
chkRN :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkRN k l a r = case r of
                E         -> error "chkRN: Bug0"     -- impossible if BF<>0
                N _ _ _ _ -> N k l a r               -- BF +/-1 -> -1, so dH= 0
                Z _ _ _ _ -> Z k l a r               -- BF +/-1 ->  0, so dH=-1
                P _ _ _ _ -> N k l a r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (Z k l a r),
-- where r was (N rk rl ra rr) or (P rk rl ra rr)
chkRZ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkRZ k l a r = case r of
                E         -> error "chkRZ: Bug0"    -- impossible if BF<>0
                N _ _ _ _ -> Z k l a r              -- BF +/-1 -> -1, so dH= 0
                Z _ _ _ _ -> P k l a r              -- BF +/-1 ->  0, so dH=-1
                P _ _ _ _ -> Z k l a r              -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (P k l a r),
-- where l was (N rk rl ra rr) or (P rk rl ra rr)
chkRP :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkRP k l a r = case r of
                E         -> error "chkRP: Bug0"    -- impossible if BF<>0
                N _ _ _ _ -> P k l a r              -- BF +/-1 -> -1, so dH= 0
                Z _ _ _ _ -> rebalP k l a r         -- BF +/-1 ->  0, so dH=-1
                P _ _ _ _ -> P k l a r              -- BF +/-1 -> +1, so dH= 0


-- Substitute deleted element from (N _ l _ r)
subN :: IntMap a -> IntMap a -> IntMap a
subN _  E               = error "subN: Bug0"      -- Impossible
subN l (N rk rl ra rr)  = case popLN rk rl ra rr of (# iv,v,r_ #) -> chkRN  iv l v r_
subN l (Z rk rl ra rr)  = case popLZ rk rl ra rr of (# iv,v,r_ #) -> chkRN_ iv l v r_
subN l (P rk rl ra rr)  = case popLP rk rl ra rr of (# iv,v,r_ #) -> chkRN  iv l v r_

-- Substitute deleted element from (Z _ l _ r)
-- Pops the replacement from the right sub-tree, so result may be (P _ _ _)
subZR :: IntMap a -> IntMap a -> IntMap a
subZR _  E               = E   -- Both left and right subtrees must have been empty
subZR l (N rk rl ra rr)  = case popLN rk rl ra rr of (# iv,v,r_ #) -> chkRZ  iv l v r_
subZR l (Z rk rl ra rr)  = case popLZ rk rl ra rr of (# iv,v,r_ #) -> chkRZ_ iv l v r_
subZR l (P rk rl ra rr)  = case popLP rk rl ra rr of (# iv,v,r_ #) -> chkRZ  iv l v r_

-- Local utility to substitute deleted element from (Z _ l _ r)
-- Pops the replacement from the left sub-tree, so result may be (N _ _ _)
subZL :: IntMap a -> IntMap a -> IntMap a
subZL  E              _  = E   -- Both left and right subtrees must have been empty
subZL (N lk ll la lr) r  = case popRN lk ll la lr of (# l_,iv,v #) -> chkLZ  iv l_ v r
subZL (Z lk ll la lr) r  = case popRZ lk ll la lr of (# l_,iv,v #) -> chkLZ_ iv l_ v r
subZL (P lk ll la lr) r  = case popRP lk ll la lr of (# l_,iv,v #) -> chkLZ  iv l_ v r

-- Substitute deleted element from (P _ l _ r)
subP :: IntMap a -> IntMap a -> IntMap a
subP  E              _  = error "subP: Bug0"      -- Impossible
subP (N lk ll la lr) r  = case popRN lk ll la lr of (# l_,iv,v #) -> chkLP  iv l_ v r
subP (Z lk ll la lr) r  = case popRZ lk ll la lr of (# l_,iv,v #) -> chkLP_ iv l_ v r
subP (P lk ll la lr) r  = case popRP lk ll la lr of (# l_,iv,v #) -> chkLP  iv l_ v r

-- Check for height changes in left subtree of (N k l a r),
-- where l was (Z lk ll la lr)
chkLN_ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkLN_ k l a r = case l of
                 E       -> rebalN k l a r  -- BF 0 -> E, so dH=-1
                 _       -> N k l a r       -- Otherwise dH=0
-- Check for height changes in left subtree of (Z k l a r),
-- where l was (Z lk ll la lr)
chkLZ_ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkLZ_ k l a r = case l of
                 E       -> N k l a r      -- BF 0 -> E, so dH=-1
                 _       -> Z k l a r      -- Otherwise dH=0
-- Check for height changes in left subtree of (P k l a r),
-- where l was (Z lk ll la lr)
chkLP_ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkLP_ k l a r = case l of
                 E       -> Z k l a r      -- BF 0 -> E, so dH=-1
                 _       -> P k l a r      -- Otherwise dH=0
-- Check for height changes in right subtree of (N k l a r),
-- where r was (Z lk rl ra rr)
chkRN_ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkRN_ k l a r = case r of
                 E       -> Z k l a r      -- BF 0 -> E, so dH=-1
                 _       -> N k l a r      -- Otherwise dH=0
-- Check for height changes in right subtree of (Z k l a r),
-- where r was (Z lk rl ra rr)
chkRZ_ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkRZ_ k l a r = case r of
                 E       -> P k l a r      -- BF 0 -> E, so dH=-1
                 _       -> Z k l a r      -- Otherwise dH=0
-- Check for height changes in right subtree of (P k l a r),
-- where l was (Z lk rl ra rr)
chkRP_ :: IntKey -> IntMap a -> a -> IntMap a -> IntMap a
chkRP_ k l a r = case r of
                 E       -> rebalP k l a r -- BF 0 -> E, so dH=-1
                 _       -> P k l a r      -- Otherwise dH=0

--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance (Eq a) => Eq (IntMap a) where
 imp0 == imp1 = asIAList imp0 == asIAList imp1

---------
-- Ord --
---------
instance Ord a => Ord (IntMap a) where
 compare imp0 imp1 = compare (asIAList imp0) (asIAList imp1)

----------
-- Show --
----------
instance Show a => Show (IntMap a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocsAsc " . shows (assocsAsc mp)

----------
-- Read --
----------

instance R.Read a => R.Read (IntMap a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocsAsc" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocsAsc xs)
 readListPrec = R.readListPrecDefault







------------------------
-- Typeable/Typeable1 --
------------------------
instance Typeable1 IntMap where
 typeOf1 _ = mkTyConApp (mkTyCon "Data.GMap.IntMap.IntMap") []
--------------
instance Typeable a => Typeable (IntMap a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance Functor IntMap where
-- fmap :: (a -> b) -> IntMap a -> IntMap b
   fmap = mapIntMap -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance M.Monoid a => M.Monoid (IntMap a) where
-- mempty :: IntMap a
   mempty = emptyIntMap
-- mappend :: IntMap a -> IntMap a -> IntMap a
   mappend map0 map1 = unionIntMap M.mappend map0 map1
-- mconcat :: [IntMap a] -> IntMap a
   mconcat maps = L.foldr (unionIntMap M.mappend) emptyIntMap maps

-------------------
-- Data.Foldable --
-------------------
instance F.Foldable IntMap where
-- fold :: Monoid m => IntMap m -> m
   fold mp = foldElemsAscIntMap M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> IntMap a -> m
   foldMap f mp = foldElemsAscIntMap (\a b -> M.mappend (f a) b) M.mempty mp
-- foldr :: (a -> b -> b) -> b -> IntMap a -> b
   foldr f b0 mp = foldElemsAscIntMap f b0 mp
-- foldl :: (a -> b -> a) -> a -> IntMap b -> a
   foldl f b0 mp = foldElemsDescIntMap (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- fold1 :: (a -> a -> a) -> IntMap a -> a
   fold1 = undefined
-- foldl1 :: (a -> a -> a) -> IntMap a -> a
   foldl1 = undefined
-}

{- ??
data IntMap a = E                                              -- ^ Empty IntMap
             | N {-# UNPACK #-} !IntKey (IntMap a) a (IntMap a)    -- ^ BF=-1 (right height > left height)
             | Z {-# UNPACK #-} !IntKey (IntMap a) a (IntMap a)    -- ^ BF= 0
             | P {-# UNPACK #-} !IntKey (IntMap a) a (IntMap a)    -- ^ BF=+1 (left height > right height)
-}



---- ToDo: Tidy This Stuff up later --
vennIntMap :: (a -> b -> c) -> IntMap a -> IntMap b -> (IntMap a, IntMap c, IntMap b)
vennIntMap f = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E            t1             = (E ,E,t1)
 gu t0                   E          = (t0,E,E )
 gu t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# l1)
 gu t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 1# l1)
 gu t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# r1)
 gu t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# l1)
 gu t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 1# l1)
 gu t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# r1)
 gu t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# l1)
 gu t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 1# l1)
 gu t0@(P _ _  _ r0) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# r1)
 gu_ t0 h0 t1 h1 = case vennH f Empt 0# t0 h0 t1 h1 of
                   (# tab,_,cs,cl,tba,_ #) -> case subst (rep (I# cl)) cs of (# tc,_ #) -> (tab,tc,tba)

vennH :: (a -> b -> c) -> IAList c -> Int# -> IntMap a -> Int# -> IntMap b -> Int# -> (# IntMap a,Int#,IAList c,Int#,IntMap b,Int# #)
vennH f = v where
 -- v :: IAList c -> Int# -> IntMap a -> Int# -> IntMap b -> Int# -> (# IntMap a,Int#,IAList c,Int#,IntMap b,Int# #)
 v cs cl  E          ha  tb         hb = (# E ,ha,cs,cl,tb,hb #)
 v cs cl  ta         ha  E          hb = (# ta,ha,cs,cl,E ,hb #)
 v cs cl (N ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (N ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (N ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#2#)
 v cs cl (Z ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (Z ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (Z ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#2#)
 v cs cl (P ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (P ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (P ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#1#) b rb (hb-#2#)
 v_ cs cl ka la hla a ra hra kb lb hlb b rb hrb =
  case compareInt# ka kb of
  -- a < b, so (la < a < b) & (a < b < rb)
  LT ->                                 case forkVenn ka lb hlb of
   (# llb,hllb,mybb,rlb,hrlb #)      -> case forkVenn kb ra hra of
    (# lra,hlra,myba,rra,hrra #)     ->
     -- (la + llb) < a < (lra + rlb) < b < (rra + rb)
                                           case v cs cl rra hrra rb hrb of
     (# rab,hrab,cs0,cl0,rba,hrba #)    -> case (case myba of
                                                 Nothing -> case v         cs0   cl0      lra hlra rlb hrlb of
                                                  (# mab,hmab,cs1,cl1,mba,hmba #) -> case spliceH kb mba hmba b rba hrba of
                                                   (# mrba,hmrba #)               -> (# mab,hmab,cs1,cl1,mrba,hmrba #)
                                                 Just a_ -> case (let c = f a_ b
                                                                  in v (Cons kb c cs0) (cl0+#1#) lra hlra rlb hrlb
                                                                 ) of
                                                  (# mab,hmab,cs1,cl1,mba,hmba #) -> case joinH   mba hmba   rba hrba of
                                                   (# mrba,hmrba #)               -> (# mab,hmab,cs1,cl1,mrba,hmrba #)
                                                ) of
      (# mab,hmab,cs1,cl1,mrba,hmrba #) -> case joinH mab hmab rab hrab of
       (# mrab,hmrab #)                 -> case (case mybb of
                                                 Nothing -> case v         cs1   cl1      la hla llb hllb of
                                                  (# lab,hlab,cs2,cl2,lba,hlba #) -> case spliceH ka lab hlab a mrab hmrab of
                                                   (# ab,hab #)                   -> (# ab,hab,cs2,cl2,lba,hlba #)
                                                 Just b_ -> case (let c = f a b_
                                                                  in v (Cons ka c cs1) (cl1+#1#) la hla llb hllb
                                                                 ) of
                                                  (# lab,hlab,cs2,cl2,lba,hlba #) -> case joinH   lab hlab   mrab hmrab of
                                                   (# ab,hab #)                   -> (# ab,hab,cs2,cl2,lba,hlba #)
                                                ) of
        (# ab,hab,cs2,cl2,lba,hlba #)   -> case joinH lba hlba mrba hmrba of
         (# ba,hba #)                   -> (# ab,hab,cs2,cl2,ba,hba #)
  -- a = b
  EQ ->                                case v    cs           cl   ra hra rb hrb of
   (# rab,hrab,cs0,cl0,rba,hrba #)  -> case (let c = f a b
                                             in v (Cons ka c cs0) (cl0+#1#) la hla lb hlb
                                            ) of
    (# lab,hlab,cs1,cl1,lba,hlba #) -> case joinH lab hlab rab hrab of
     (# ab,hab #)                   -> case joinH lba hlba rba hrba of
      (# ba,hba #)                  -> (# ab,hab,cs1,cl1,ba,hba #)
  -- b < a, so (lb < b < a) & (b < a < ra)
  GT ->                                  case forkVenn ka rb hrb of
   (# lrb,hlrb,mybb,rrb,hrrb #)       -> case forkVenn kb la hla of
    (# lla,hlla,myba,rla,hrla #)      ->
     -- (lla + lb) < b < (rla + lrb) < a < (ra + rrb)
                                            case v cs cl ra hra rrb hrrb of
     (# rab,hrab,cs0,cl0,rba,hrba #)     -> case (case mybb of
                                                  Nothing -> case v         cs0   cl0      rla hrla lrb hlrb of
                                                   (# mab,hmab,cs1,cl1,mba,hmba #) -> case spliceH ka mab hmab a rab hrab of
                                                    (# mrab,hmrab #)               -> (# mrab,hmrab,cs1,cl1,mba,hmba #)
                                                  Just b_ -> case (let c = f a b_
                                                                   in v (Cons ka c cs0) (cl0+#1#) rla hrla lrb hlrb
                                                                  ) of
                                                   (# mab,hmab,cs1,cl1,mba,hmba #) -> case joinH   mab hmab   rab hrab of
                                                    (# mrab,hmrab #)               -> (# mrab,hmrab,cs1,cl1,mba,hmba #)
                                                 ) of
      (# mrab,hmrab,cs1,cl1,mba,hmba #)  -> case joinH mba hmba rba hrba of
       (# mrba,hmrba #)                  -> case (case myba of
                                                  Nothing -> case v         cs1   cl1      lla hlla lb hlb of
                                                   (# lab,hlab,cs2,cl2,lba,hlba #) -> case spliceH kb lba hlba b mrba hmrba of
                                                    (# ba,hba #)                   -> (# lab,hlab,cs2,cl2,ba,hba #)
                                                  Just a_ -> case (let c = f a_ b
                                                                   in v (Cons kb c cs1) (cl1+#1#) lla hlla lb hlb
                                                                  ) of
                                                   (# lab,hlab,cs2,cl2,lba,hlba #) -> case joinH   lba hlba   mrba hmrba of
                                                    (# ba,hba #)                   -> (# lab,hlab,cs2,cl2,ba,hba #)
                                                 ) of
        (# lab,hlab,cs2,cl2,ba,hba #)    -> case joinH lab hlab mrab hmrab of
         (# ab,hab #)                    -> (# ab,hab,cs2,cl2,ba,hba #)
-----------------------------------------------------------------------
-------------------------- vennH Ends Here ----------------------------
-----------------------------------------------------------------------

vennIntMap' :: (a -> b -> c) -> IntMap a -> IntMap b -> (IntMap a, IntMap c, IntMap b)
vennIntMap' f = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E            t1             = (E ,E,t1)
 gu t0                   E          = (t0,E,E )
 gu t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# l1)
 gu t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 1# l1)
 gu t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# r1)
 gu t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# l1)
 gu t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 1# l1)
 gu t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# r1)
 gu t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# l1)
 gu t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 1# l1)
 gu t0@(P _ _  _ r0) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# r1)
 gu_ t0 h0 t1 h1 = case vennH' f Empt 0# t0 h0 t1 h1 of
                   (# tab,_,cs,cl,tba,_ #) -> case subst (rep (I# cl)) cs of (# tc,_ #) -> (tab,tc,tba)
-- Strict version of vennH
vennH' :: (a -> b -> c) -> IAList c -> Int# -> IntMap a -> Int# -> IntMap b -> Int# -> (# IntMap a,Int#,IAList c,Int#,IntMap b,Int# #)
vennH' f = v where
 -- v :: IAList c -> Int# -> IntMap a -> Int# -> IntMap b -> Int# -> (# IntMap a,Int#,IAList c,Int#,IntMap b,Int# #)
 v cs cl  E          ha  tb         hb = (# E ,ha,cs,cl,tb,hb #)
 v cs cl  ta         ha  E          hb = (# ta,ha,cs,cl,E ,hb #)
 v cs cl (N ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (N ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (N ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#2#)
 v cs cl (Z ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (Z ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (Z ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#2#)
 v cs cl (P ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (P ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (P ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#1#) b rb (hb-#2#)
 v_ cs cl ka la hla a ra hra kb lb hlb b rb hrb =
  case compareInt# ka kb of
  -- a < b, so (la < a < b) & (a < b < rb)
  LT ->                                 case forkVenn ka lb hlb of
   (# llb,hllb,mybb,rlb,hrlb #)      -> case forkVenn kb ra hra of
    (# lra,hlra,myba,rra,hrra #)     ->
     -- (la + llb) < a < (lra + rlb) < b < (rra + rb)
                                           case v cs cl rra hrra rb hrb of
     (# rab,hrab,cs0,cl0,rba,hrba #)    -> case (case myba of
                                                 Nothing -> case v         cs0   cl0      lra hlra rlb hrlb of
                                                  (# mab,hmab,cs1,cl1,mba,hmba #) -> case spliceH kb mba hmba b rba hrba of
                                                   (# mrba,hmrba #)               -> (# mab,hmab,cs1,cl1,mrba,hmrba #)
                                                 Just a_ -> case (let c = f a_ b
                                                                  in c `seq` v (Cons kb c cs0) (cl0+#1#) lra hlra rlb hrlb
                                                                 ) of
                                                  (# mab,hmab,cs1,cl1,mba,hmba #) -> case joinH   mba hmba   rba hrba of
                                                   (# mrba,hmrba #)               -> (# mab,hmab,cs1,cl1,mrba,hmrba #)
                                                ) of
      (# mab,hmab,cs1,cl1,mrba,hmrba #) -> case joinH mab hmab rab hrab of
       (# mrab,hmrab #)                 -> case (case mybb of
                                                 Nothing -> case v         cs1   cl1      la hla llb hllb of
                                                  (# lab,hlab,cs2,cl2,lba,hlba #) -> case spliceH ka lab hlab a mrab hmrab of
                                                   (# ab,hab #)                   -> (# ab,hab,cs2,cl2,lba,hlba #)
                                                 Just b_ -> case (let c = f a b_
                                                                  in c `seq` v (Cons ka c cs1) (cl1+#1#) la hla llb hllb
                                                                 ) of
                                                  (# lab,hlab,cs2,cl2,lba,hlba #) -> case joinH   lab hlab   mrab hmrab of
                                                   (# ab,hab #)                   -> (# ab,hab,cs2,cl2,lba,hlba #)
                                                ) of
        (# ab,hab,cs2,cl2,lba,hlba #)   -> case joinH lba hlba mrba hmrba of
         (# ba,hba #)                   -> (# ab,hab,cs2,cl2,ba,hba #)
  -- a = b
  EQ ->                                case v    cs           cl   ra hra rb hrb of
   (# rab,hrab,cs0,cl0,rba,hrba #)  -> case (let c = f a b
                                             in c `seq` v (Cons ka c cs0) (cl0+#1#) la hla lb hlb
                                            ) of
    (# lab,hlab,cs1,cl1,lba,hlba #) -> case joinH lab hlab rab hrab of
     (# ab,hab #)                   -> case joinH lba hlba rba hrba of
      (# ba,hba #)                  -> (# ab,hab,cs1,cl1,ba,hba #)
  -- b < a, so (lb < b < a) & (b < a < ra)
  GT ->                                  case forkVenn ka rb hrb of
   (# lrb,hlrb,mybb,rrb,hrrb #)       -> case forkVenn kb la hla of
    (# lla,hlla,myba,rla,hrla #)      ->
     -- (lla + lb) < b < (rla + lrb) < a < (ra + rrb)
                                            case v cs cl ra hra rrb hrrb of
     (# rab,hrab,cs0,cl0,rba,hrba #)     -> case (case mybb of
                                                  Nothing -> case v         cs0   cl0      rla hrla lrb hlrb of
                                                   (# mab,hmab,cs1,cl1,mba,hmba #) -> case spliceH ka mab hmab a rab hrab of
                                                    (# mrab,hmrab #)               -> (# mrab,hmrab,cs1,cl1,mba,hmba #)
                                                  Just b_ -> case (let c = f a b_
                                                                   in c `seq` v (Cons ka c cs0) (cl0+#1#) rla hrla lrb hlrb
                                                                  ) of
                                                   (# mab,hmab,cs1,cl1,mba,hmba #) -> case joinH   mab hmab   rab hrab of
                                                    (# mrab,hmrab #)               -> (# mrab,hmrab,cs1,cl1,mba,hmba #)
                                                 ) of
      (# mrab,hmrab,cs1,cl1,mba,hmba #)  -> case joinH mba hmba rba hrba of
       (# mrba,hmrba #)                  -> case (case myba of
                                                  Nothing -> case v         cs1   cl1      lla hlla lb hlb of
                                                   (# lab,hlab,cs2,cl2,lba,hlba #) -> case spliceH kb lba hlba b mrba hmrba of
                                                    (# ba,hba #)                   -> (# lab,hlab,cs2,cl2,ba,hba #)
                                                  Just a_ -> case (let c = f a_ b
                                                                   in c `seq` v (Cons kb c cs1) (cl1+#1#) lla hlla lb hlb
                                                                  ) of
                                                   (# lab,hlab,cs2,cl2,lba,hlba #) -> case joinH   lba hlba   mrba hmrba of
                                                    (# ba,hba #)                   -> (# lab,hlab,cs2,cl2,ba,hba #)
                                                 ) of
        (# lab,hlab,cs2,cl2,ba,hba #)    -> case joinH lab hlab mrab hmrab of
         (# ab,hab #)                    -> (# ab,hab,cs2,cl2,ba,hba #)
-----------------------------------------------------------------------
-------------------------- vennH' Ends Here ---------------------------
-----------------------------------------------------------------------


vennMaybeIntMap :: (a -> b -> Maybe c) -> IntMap a -> IntMap b -> (IntMap a, IntMap c, IntMap b)
vennMaybeIntMap f = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E            t1             = (E ,E,t1)
 gu t0                   E          = (t0,E,E )
 gu t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# l1)
 gu t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 1# l1)
 gu t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# r1)
 gu t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# l1)
 gu t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 1# l1)
 gu t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# r1)
 gu t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# l1)
 gu t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 1# l1)
 gu t0@(P _ _  _ r0) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# r1)
 gu_ t0 h0 t1 h1 = case vennMaybeH f Empt 0# t0 h0 t1 h1 of
                   (# tab,_,cs,cl,tba,_ #) -> case subst (rep (I# cl)) cs of (# tc,_ #) -> (tab,tc,tba)
vennMaybeH :: (a -> b -> Maybe c) -> IAList c -> Int# -> IntMap a -> Int# -> IntMap b -> Int# -> (# IntMap a,Int#,IAList c,Int#,IntMap b,Int# #)
vennMaybeH f = v where
 -- v :: IAList c -> Int# -> IntMap a -> Int# -> IntMap b -> Int# -> (# IntMap a,Int#,IAList c,Int#,IntMap b,Int# #)
 v cs cl  E          ha  tb         hb = (# E ,ha,cs,cl,tb,hb #)
 v cs cl  ta         ha  E          hb = (# ta,ha,cs,cl,E ,hb #)
 v cs cl (N ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (N ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (N ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#2#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#2#)
 v cs cl (Z ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (Z ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (Z ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#1#) kb lb (hb-#1#) b rb (hb-#2#)
 v cs cl (P ka la a ra) ha (N kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#2#) b rb (hb-#1#)
 v cs cl (P ka la a ra) ha (Z kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#1#) b rb (hb-#1#)
 v cs cl (P ka la a ra) ha (P kb lb b rb) hb = v_ cs cl ka la (ha-#1#) a ra (ha-#2#) kb lb (hb-#1#) b rb (hb-#2#)
 v_ cs cl ka la hla a ra hra kb lb hlb b rb hrb =
  case compareInt# ka kb of
  -- a < b, so (la < a < b) & (a < b < rb)
  LT ->                                 case forkVenn ka lb hlb of
   (# llb,hllb,mybb,rlb,hrlb #)      -> case forkVenn kb ra hra of
    (# lra,hlra,myba,rra,hrra #)     ->
     -- (la + llb) < a < (lra + rlb) < b < (rra + rb)
                                           case v cs cl rra hrra rb hrb of
     (# rab,hrab,cs0,cl0,rba,hrba #)    -> case (case myba of
                                                 Nothing -> case v            cs0   cl0      lra hlra rlb hrlb of
                                                  (# mab,hmab,cs1,cl1,mba,hmba #) -> case spliceH kb mba hmba b rba hrba of
                                                   (# mrba,hmrba #)               -> (# mab,hmab,cs1,cl1,mrba,hmrba #)
                                                 Just a_ -> case (case f a_ b of
                                                                  Nothing -> v            cs0   cl0      lra hlra rlb hrlb
                                                                  Just c  -> v (Cons kb c cs0) (cl0+#1#) lra hlra rlb hrlb
                                                                 ) of
                                                  (# mab,hmab,cs1,cl1,mba,hmba #) -> case joinH   mba hmba   rba hrba of
                                                   (# mrba,hmrba #)               -> (# mab,hmab,cs1,cl1,mrba,hmrba #)
                                                ) of
      (# mab,hmab,cs1,cl1,mrba,hmrba #) -> case joinH mab hmab rab hrab of
       (# mrab,hmrab #)                 -> case (case mybb of
                                                 Nothing -> case v            cs1   cl1      la hla llb hllb of
                                                  (# lab,hlab,cs2,cl2,lba,hlba #) -> case spliceH ka lab hlab a mrab hmrab of
                                                   (# ab,hab #)                   -> (# ab,hab,cs2,cl2,lba,hlba #)
                                                 Just b_ -> case (case f a b_ of
                                                                  Nothing -> v            cs1   cl1      la hla llb hllb
                                                                  Just c  -> v (Cons ka c cs1) (cl1+#1#) la hla llb hllb
                                                                 ) of
                                                  (# lab,hlab,cs2,cl2,lba,hlba #) -> case joinH   lab hlab   mrab hmrab of
                                                   (# ab,hab #)                   -> (# ab,hab,cs2,cl2,lba,hlba #)
                                                ) of
        (# ab,hab,cs2,cl2,lba,hlba #)   -> case joinH lba hlba mrba hmrba of
         (# ba,hba #)                   -> (# ab,hab,cs2,cl2,ba,hba #)
  -- a = b
  EQ ->                                case v    cs           cl   ra hra rb hrb of
   (# rab,hrab,cs0,cl0,rba,hrba #)  -> case (case f a b of
                                             Nothing -> v            cs0   cl0      la hla lb hlb
                                             Just c  -> v (Cons ka c cs0) (cl0+#1#) la hla lb hlb
                                            ) of
    (# lab,hlab,cs1,cl1,lba,hlba #) -> case joinH lab hlab rab hrab of
     (# ab,hab #)                   -> case joinH lba hlba rba hrba of
      (# ba,hba #)                  -> (# ab,hab,cs1,cl1,ba,hba #)
  -- b < a, so (lb < b < a) & (b < a < ra)
  GT ->                                  case forkVenn ka rb hrb of
   (# lrb,hlrb,mybb,rrb,hrrb #)       -> case forkVenn kb la hla of
    (# lla,hlla,myba,rla,hrla #)      ->
     -- (lla + lb) < b < (rla + lrb) < a < (ra + rrb)
                                            case v cs cl ra hra rrb hrrb of
     (# rab,hrab,cs0,cl0,rba,hrba #)     -> case (case mybb of
                                                  Nothing -> case v            cs0   cl0      rla hrla lrb hlrb of
                                                   (# mab,hmab,cs1,cl1,mba,hmba #) -> case spliceH ka mab hmab a rab hrab of
                                                    (# mrab,hmrab #)               -> (# mrab,hmrab,cs1,cl1,mba,hmba #)
                                                  Just b_ -> case (case f a b_ of
                                                                   Nothing -> v            cs0   cl0      rla hrla lrb hlrb
                                                                   Just c  -> v (Cons ka c cs0) (cl0+#1#) rla hrla lrb hlrb
                                                                  ) of
                                                   (# mab,hmab,cs1,cl1,mba,hmba #) -> case joinH   mab hmab   rab hrab of
                                                    (# mrab,hmrab #)               -> (# mrab,hmrab,cs1,cl1,mba,hmba #)
                                                 ) of
      (# mrab,hmrab,cs1,cl1,mba,hmba #)  -> case joinH mba hmba rba hrba of
       (# mrba,hmrba #)                  -> case (case myba of
                                                  Nothing -> case v            cs1   cl1      lla hlla lb hlb of
                                                   (# lab,hlab,cs2,cl2,lba,hlba #) -> case spliceH kb lba hlba b mrba hmrba of
                                                    (# ba,hba #)                   -> (# lab,hlab,cs2,cl2,ba,hba #)
                                                  Just a_ -> case (case f a_ b of
                                                                   Nothing -> v            cs1   cl1      lla hlla lb hlb
                                                                   Just c  -> v (Cons kb c cs1) (cl1+#1#) lla hlla lb hlb
                                                                  ) of
                                                   (# lab,hlab,cs2,cl2,lba,hlba #) -> case joinH   lba hlba   mrba hmrba of
                                                    (# ba,hba #)                   -> (# lab,hlab,cs2,cl2,ba,hba #)
                                                 ) of
        (# lab,hlab,cs2,cl2,ba,hba #)    -> case joinH lab hlab mrab hmrab of
         (# ab,hab #)                    -> (# ab,hab,cs2,cl2,ba,hba #)
-----------------------------------------------------------------------
------------------------ vennMaybeH Ends Here -------------------------
-----------------------------------------------------------------------

-- Common fork for Vennops
forkVenn :: IntKey -> IntMap a -> Int# -> (# IntMap a,Int#,Maybe a,IntMap a,Int# #)
forkVenn k ta hta = f ta hta where
 f  E           h = (# E,h,Nothing,E,h #)
 f (N ka l a r) h = f_ ka l (h-#2#) a r (h-#1#)
 f (Z ka l a r) h = f_ ka l (h-#1#) a r (h-#1#)
 f (P ka l a r) h = f_ ka l (h-#1#) a r (h-#2#)
 f_ ka l hl a r hr = case compareInt# k ka of
                     LT ->                            case f l hl of
                           (# ll,hll,mba,lr,hlr #) -> case spliceH ka lr hlr a r hr of
                            (# r_,hr_ #)           -> (# ll,hll,mba,r_,hr_ #)
                     EQ -> (# l,hl,Just a,r,hr #)
                     GT ->                            case f r hr of
                           (# rl,hrl,mbc,rr,hrr #) -> case spliceH ka l hl a rl hrl of
                            (# l_,hl_ #)           -> (# l_,hl_,mbc,rr,hrr #)


disjointUnionIntMap :: IntMap a -> IntMap a -> IntMap a
disjointUnionIntMap = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E            t1               = t1
 gu t0                   E            = t0
 gu t0@(N _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# l1)
 gu t0@(N _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# l0) t1 (addHeight 1# l1)
 gu t0@(N _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# l0) t1 (addHeight 2# r1)
 gu t0@(Z _ l0 _ _ ) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# l1)
 gu t0@(Z _ l0 _ _ ) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 1# l0) t1 (addHeight 1# l1)
 gu t0@(Z _ l0 _ _ ) t1@(P _ _  _ r1) = gu_ t0 (addHeight 1# l0) t1 (addHeight 2# r1)
 gu t0@(P _ _  _ r0) t1@(N _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# l1)
 gu t0@(P _ _  _ r0) t1@(Z _ l1 _ _ ) = gu_ t0 (addHeight 2# r0) t1 (addHeight 1# l1)
 gu t0@(P _ _  _ r0) t1@(P _ _  _ r1) = gu_ t0 (addHeight 2# r0) t1 (addHeight 2# r1)
 gu_ t0 h0 t1 h1 = case disjointUnionH t0 h0 t1 h1 of (# t,_ #) -> t
disjointUnionH :: IntMap a -> Int# -> IntMap a -> Int# -> (# IntMap a,Int# #)
disjointUnionH = u where
 -- u :: IntMap a -> UINT -> IntMap a -> UINT -> (# IntMap a,UINT #)
 u  E              _   t1             h1 = (# t1,h1 #)
 u  t0             h0  E              _  = (# t0,h0 #)
 u (N k0 l0 e0 r0) h0 (N k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#2#) e0 r0 (h0-#1#) k1 l1 (h1-#2#) e1 r1 (h1-#1#)
 u (N k0 l0 e0 r0) h0 (Z k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#2#) e0 r0 (h0-#1#) k1 l1 (h1-#1#) e1 r1 (h1-#1#)
 u (N k0 l0 e0 r0) h0 (P k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#2#) e0 r0 (h0-#1#) k1 l1 (h1-#1#) e1 r1 (h1-#2#)
 u (Z k0 l0 e0 r0) h0 (N k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#1#) e0 r0 (h0-#1#) k1 l1 (h1-#2#) e1 r1 (h1-#1#)
 u (Z k0 l0 e0 r0) h0 (Z k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#1#) e0 r0 (h0-#1#) k1 l1 (h1-#1#) e1 r1 (h1-#1#)
 u (Z k0 l0 e0 r0) h0 (P k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#1#) e0 r0 (h0-#1#) k1 l1 (h1-#1#) e1 r1 (h1-#2#)
 u (P k0 l0 e0 r0) h0 (N k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#1#) e0 r0 (h0-#2#) k1 l1 (h1-#2#) e1 r1 (h1-#1#)
 u (P k0 l0 e0 r0) h0 (Z k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#1#) e0 r0 (h0-#2#) k1 l1 (h1-#1#) e1 r1 (h1-#1#)
 u (P k0 l0 e0 r0) h0 (P k1 l1 e1 r1) h1 = u_ k0 l0 (h0-#1#) e0 r0 (h0-#2#) k1 l1 (h1-#1#) e1 r1 (h1-#2#)
 u_ k0 l0 hl0 e0 r0 hr0 k1 l1 hl1 e1 r1 hr1 =
  case compareInt# k0 k1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  LT ->                             case fork k1 r0 hr0 of
        (# rl0,hrl0,rr0,hrr0 #)  -> case fork k0 l1 hl1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
         (# ll1,hll1,lr1,hlr1 #) ->                        -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
          -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                    case u  l0  hl0 ll1 hll1 of
          (# l,hl #)             -> case u rl0 hrl0 lr1 hlr1 of
           (# m,hm #)            -> case u rr0 hrr0  r1  hr1 of
            (# r,hr #)           -> case spliceH k1 m hm e1 r hr of
             (# t,ht #)          -> spliceH k0 l hl e0 t ht
  -- e0 = e1
  EQ -> error "disjointUnionH: Trees intersect" `seq` (# E,0# #)
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                             case fork k0 r1 hr1 of
        (# rl1,hrl1,rr1,hrr1 #)  -> case fork k1 l0 hl0 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
         (# ll0,hll0,lr0,hlr0 #) ->                        -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
          -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                    case u ll0 hll0  l1  hl1 of
          (# l,hl #)             -> case u lr0 hlr0 rl1 hrl1 of
           (# m,hm #)            -> case u  r0  hr0 rr1 hrr1 of
            (# r,hr #)           -> case spliceH k1 l hl e1 m hm of
             (# t,ht #)          -> spliceH k0 t ht e0 r hr
 -- fork :: IntKey -> IntMap a -> Int# -> (# IntMap a,Int#,IntMap a,Int# #)
 fork k0 t1 ht1 = fork_ t1 ht1 where
  fork_  E        _ = (# E,0#,E,0# #)
  fork_ (N k l e r) h = fork__ k l (h-#2#) e r (h-#1#)
  fork_ (Z k l e r) h = fork__ k l (h-#1#) e r (h-#1#)
  fork_ (P k l e r) h = fork__ k l (h-#1#) e r (h-#2#)
  fork__ k l hl e r hr = case compareInt# k0 k of
                         LT ->                        case fork_ l hl of
                               (# l0,hl0,l1,hl1 #) -> case spliceH k l1 hl1 e r hr of
                                (# l1_,hl1_ #)     -> (# l0,hl0,l1_,hl1_ #)
                         EQ -> error "disjointUnionH: Trees intersect" `seq` (# E,0#,E,0# #)
                         GT ->                        case fork_ r hr of
                               (# l0,hl0,l1,hl1 #) -> case spliceH k l hl e l0 hl0 of
                                (# l0_,hl0_ #)     -> (# l0_,hl0_,l1,hl1 #)
-----------------------------------------------------------------------
---------------------- disjointUnionH Ends Here -----------------------
-----------------------------------------------------------------------
