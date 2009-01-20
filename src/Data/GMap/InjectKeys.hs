{-# OPTIONS_GHC -fglasgow-exts -Wall -fno-warn-missing-signatures -fno-monomorphism-restriction #-}

module Data.GMap.InjectKeys
(-- * InjectKeys type
 InjectKeys
,Injection
,K1
,inject
,outject
) where

import Prelude hiding (foldr,map,filter,lookup)
import Data.GMap

import Data.Typeable
import qualified Data.Foldable as F
import qualified Data.Monoid as M
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import Data.Maybe hiding (mapMaybe)

import GHC.Base hiding (map)
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

import qualified Data.List as L

--------------------------------------------------------------------------------------------
--                     Used when keys can be transformed into the key type of an existing maps
--		       eg. to store Enums in an IntMap
--------------------------------------------------------------------------------------------

-- | The k2 parameter is the key of the map. This is redundant and can be removed once ghc properly supports
-- equality constraints in superclasses
data InjectKeys t k2 mp a = InjectKeys !(mp a)

-- | 't' is a phantom type which determines the encoding and decoding functions used.
-- 't' is passed as an undefined value.
-- 'inject' must be injective (ie (inject a) == (inject b) implies a == b) and reversible by 'outject'
-- The mixture of associated types and dependent parameters can be fixed once ghc properly supports equality
-- constraints in superclasses
class Injection t k2 | t -> k2 where
	type K1 t
	inject :: t -> K1 t -> k2
	outject :: t -> k2 -> K1 t

transformOf :: InjectKeys t k2 mp a -> t
transformOf = undefined

-- Dont export these, used to force correct types
injectFor :: Injection t k2 => InjectKeys t k2 mp a -> K1 t -> k2
injectFor inj k1 = inject (transformOf inj) k1

outjectFor :: Injection t k2 => InjectKeys t k2 mp a -> k2 -> K1 t
outjectFor inj k2 = outject (transformOf inj) k2

-- | InjectKeys is an instance of Map.
-- instance (Eq (K1 t), Injection t, Map mp, K2 t ~ Key mp) => Map (InjectKeys t mp) where
instance (Eq (K1 t), Injection t k2, Map mp, k2 ~ Key mp) => Map (InjectKeys t k2 mp) where
	type Key (InjectKeys t k2 mp) = K1 t

	empty                 	= emptyInjectKeys
	singleton             	= singletonInjectKeys
	pair                  	= pairInjectKeys
	nonEmpty              	= nonEmptyInjectKeys
	status                	= statusInjectKeys
	addSize               	= addSizeInjectKeys
	lookup                	= lookupInjectKeys
	lookupCont            	= lookupContInjectKeys
	alter			= alterInjectKeys
	insertWith            	= insertWithInjectKeys 
	insertWith'           	= insertWithInjectKeys'
	insertMaybe           	= insertMaybeInjectKeys
-- 	fromAssocsWith	        = fromAssocsWithInjectKeys
-- 	fromAssocsMaybe 	= fromAssocsMaybeInjectKeys
	delete                	= deleteInjectKeys 
	adjustWith           	= adjustWithInjectKeys
	adjustWith' 		= adjustWithInjectKeys'
	adjustMaybe		= adjustMaybeInjectKeys
	venn			= vennInjectKeys
	venn'			= vennInjectKeys'
	vennMaybe		= vennMaybeInjectKeys
	disjointUnion		= disjointUnionInjectKeys
	union                 	= unionInjectKeys
	union'                	= unionInjectKeys'
	unionMaybe            	= unionMaybeInjectKeys
	intersection          	= intersectionInjectKeys
	intersection'         	= intersectionInjectKeys'
	intersectionMaybe     	= intersectionMaybeInjectKeys
	difference            	= differenceInjectKeys
	differenceMaybe       	= differenceMaybeInjectKeys
	isSubsetOf            	= isSubsetOfInjectKeys
	isSubmapOf            	= isSubmapOfInjectKeys 
	map                   	= mapInjectKeys
	map'                  	= mapInjectKeys'
	mapMaybe              	= mapMaybeInjectKeys
	mapWithKey            	= mapWithInjectionKeys
	mapWithKey'           	= mapWithInjectionKeys'
	filter                	= filterInjectKeys
	foldKeys		= foldKeysInjectKeys
	foldElems 		= foldElemsInjectKeys
	foldAssocs		= foldAssocsInjectKeys
	foldKeys'		= foldKeysInjectKeys'
	foldElems' 		= foldElemsInjectKeys'
	foldAssocs'		= foldAssocsInjectKeys'
	foldElemsUInt         	= foldElemsUIntInjectKeys
	valid                 	= validInjectKeys
 
instance (Eq (K1 t), Injection t k2, OrderedMap mp, k2 ~ Key mp) => OrderedMap (InjectKeys t k2 mp) where
	compareKey 	= compareInjectionKeys
	fromAssocsAscWith = fromAssocsAscWithInjectKeys
	fromAssocsDescWith = fromAssocsDescWithInjectKeys
	fromAssocsAscMaybe = fromAssocsAscMaybeInjectKeys
	fromAssocsDescMaybe = fromAssocsDescMaybeInjectKeys
 	foldElemsAsc	= foldElemsAscInjectKeys
	foldElemsDesc	= foldElemsDescInjectKeys
	foldKeysAsc	= foldKeysAscInjectKeys
	foldKeysDesc	= foldKeysDescInjectKeys
	foldAssocsAsc	= foldAssocsAscInjectKeys
	foldAssocsDesc	= foldAssocsDescInjectKeys
	foldElemsAsc'	= foldElemsAscInjectKeys'
	foldElemsDesc'	= foldElemsDescInjectKeys'
	foldKeysAsc'	= foldKeysAscInjectKeys'
	foldKeysDesc'	= foldKeysDescInjectKeys'
	foldAssocsAsc'	= foldAssocsAscInjectKeys'
	foldAssocsDesc'	= foldAssocsDescInjectKeys'

emptyInjectKeys = InjectKeys empty

singletonInjectKeys k a = let tk = InjectKeys (singleton (injectFor tk k) a) in tk

fromAssocsAscWithInjectKeys   f kas = let tk = InjectKeys (fromAssocsAscWith   f [(injectFor tk k,a) | (k,a) <- kas]) in tk
fromAssocsDescWithInjectKeys  f kas = let tk = InjectKeys (fromAssocsDescWith  f [(injectFor tk k,a) | (k,a) <- kas]) in tk
fromAssocsAscMaybeInjectKeys  f kas = let tk = InjectKeys (fromAssocsAscMaybe  f [(injectFor tk k,a) | (k,a) <- kas]) in tk
fromAssocsDescMaybeInjectKeys f kas = let tk = InjectKeys (fromAssocsDescMaybe f [(injectFor tk k,a) | (k,a) <- kas]) in tk

pairInjectKeys k1 k2 = 
	let 	tk = (fromJust pairf) undefined undefined -- Roundabout way of getting hold of the transform type
		pairf = 
			case pair (injectFor tk k1) (injectFor tk k2) of
				Nothing -> Nothing
				Just f -> Just (\a1 a2 -> InjectKeys (f a1 a2))
	in	pairf

nonEmptyInjectKeys (InjectKeys mp) = fmap InjectKeys (nonEmpty mp) 

statusInjectKeys tk@(InjectKeys mp) = 
	case status mp of
		None    -> None
		One k a -> One (outjectFor tk k) a
		Many    -> Many

addSizeInjectKeys (InjectKeys mp) = addSize mp

lookupInjectKeys k tk@(InjectKeys mp) = lookup (injectFor tk k) mp

lookupContInjectKeys f k tk@(InjectKeys mp) = lookupCont f (injectFor tk k) mp

alterInjectKeys  f k tk@(InjectKeys mp) = InjectKeys (alter  f (injectFor tk k) mp)

insertWithInjectKeys  f k a tk@(InjectKeys mp) = InjectKeys (insertWith  f (injectFor tk k) a mp)
insertWithInjectKeys' f k a tk@(InjectKeys mp) = InjectKeys (insertWith' f (injectFor tk k) a mp)

insertMaybeInjectKeys  f k a tk@(InjectKeys mp) = InjectKeys (insertMaybe  f (injectFor tk k) a mp)

deleteInjectKeys k tk@(InjectKeys mp) = InjectKeys (delete (injectFor tk k) mp)

adjustWithInjectKeys  f k tk@(InjectKeys mp) = InjectKeys (adjustWith  f (injectFor tk k) mp)
adjustWithInjectKeys' f k tk@(InjectKeys mp) = InjectKeys (adjustWith' f (injectFor tk k) mp)

adjustMaybeInjectKeys  f k tk@(InjectKeys mp) = InjectKeys (adjustMaybe  f (injectFor tk k) mp)

vennInjectKeys f (InjectKeys mp1) (InjectKeys mp2) = (InjectKeys leftDiff, InjectKeys inter, InjectKeys rightDiff)
 where (leftDiff, inter, rightDiff) = venn f mp1 mp2 
vennInjectKeys' f (InjectKeys mp1) (InjectKeys mp2) = (InjectKeys leftDiff, InjectKeys inter, InjectKeys rightDiff)
 where (leftDiff, inter, rightDiff) = venn' f mp1 mp2 
vennMaybeInjectKeys f (InjectKeys mp1) (InjectKeys mp2) = (InjectKeys leftDiff, InjectKeys inter, InjectKeys rightDiff)
 where (leftDiff, inter, rightDiff) = vennMaybe f mp1 mp2 

disjointUnionInjectKeys (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (disjointUnion mp1 mp2)
unionInjectKeys  f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (union  f mp1 mp2) 
unionInjectKeys' f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (union' f mp1 mp2) 

unionMaybeInjectKeys  f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (unionMaybe  f mp1 mp2) 

intersectionInjectKeys  f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (intersection  f mp1 mp2) 
intersectionInjectKeys' f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (intersection' f mp1 mp2) 

intersectionMaybeInjectKeys  f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (intersectionMaybe  f mp1 mp2) 

differenceInjectKeys (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (difference mp1 mp2) 

differenceMaybeInjectKeys  f (InjectKeys mp1) (InjectKeys mp2) = InjectKeys (differenceMaybe  f mp1 mp2) 

isSubsetOfInjectKeys   (InjectKeys mp1) (InjectKeys mp2) = isSubsetOf   mp1 mp2
isSubmapOfInjectKeys f (InjectKeys mp1) (InjectKeys mp2) = isSubmapOf f mp1 mp2

mapInjectKeys  f (InjectKeys mp) = InjectKeys (map  f mp)
mapInjectKeys' f (InjectKeys mp) = InjectKeys (map' f mp)

mapMaybeInjectKeys  f (InjectKeys mp) = InjectKeys (mapMaybe  f mp)

mapWithInjectionKeys  f tk@(InjectKeys mp) = InjectKeys (mapWithKey  (\k a -> f (outjectFor tk k) a) mp)
mapWithInjectionKeys' f tk@(InjectKeys mp) = InjectKeys (mapWithKey' (\k a -> f (outjectFor tk k) a) mp)

filterInjectKeys f (InjectKeys mp) = InjectKeys (filter f mp)

foldElemsInjectKeys   f b    (InjectKeys mp) = foldElems f b mp
foldKeysInjectKeys    f b tk@(InjectKeys mp) = foldKeys (\ k b' -> f (outjectFor tk k) b') b mp
foldAssocsInjectKeys  f b tk@(InjectKeys mp) = foldAssocs (\ k a b' -> f (outjectFor tk k) a b') b mp
foldElemsInjectKeys'  f b    (InjectKeys mp) = foldElems' f b mp
foldKeysInjectKeys'   f b tk@(InjectKeys mp) = foldKeys' (\ k b' -> f (outjectFor tk k) b') b mp
foldAssocsInjectKeys' f b tk@(InjectKeys mp) = foldAssocs' (\ k a b' -> f (outjectFor tk k) a b') b mp
foldElemsAscInjectKeys     f b    (InjectKeys mp) = foldElemsAsc f b mp
foldElemsDescInjectKeys    f b    (InjectKeys mp) = foldElemsDesc f b mp
foldKeysAscInjectKeys      f b tk@(InjectKeys mp) = foldKeysAsc (\ k b' -> f (outjectFor tk k) b') b mp
foldKeysDescInjectKeys     f b tk@(InjectKeys mp) = foldKeysDesc (\ k b' -> f (outjectFor tk k) b') b mp
foldAssocsAscInjectKeys    f b tk@(InjectKeys mp) = foldAssocsAsc (\ k a b' -> f (outjectFor tk k) a b') b mp
foldAssocsDescInjectKeys   f b tk@(InjectKeys mp) = foldAssocsDesc (\ k a b' -> f (outjectFor tk k) a b') b mp
foldElemsAscInjectKeys'    f b    (InjectKeys mp) = foldElemsAsc' f b mp
foldElemsDescInjectKeys'   f b    (InjectKeys mp) = foldElemsDesc' f b mp
foldKeysAscInjectKeys'     f b tk@(InjectKeys mp) = foldKeysAsc' (\ k b' -> f (outjectFor tk k) b') b mp
foldKeysDescInjectKeys'    f b tk@(InjectKeys mp) = foldKeysDesc' (\ k b' -> f (outjectFor tk k) b') b mp
foldAssocsAscInjectKeys'   f b tk@(InjectKeys mp) = foldAssocsAsc' (\ k a b' -> f (outjectFor tk k) a b') b mp
foldAssocsDescInjectKeys'  f b tk@(InjectKeys mp) = foldAssocsDesc' (\ k a b' -> f (outjectFor tk k) a b') b mp
foldElemsUIntInjectKeys    f b    (InjectKeys mp) = foldElemsUInt f b mp

validInjectKeys (InjectKeys mp) = valid mp

compareInjectionKeys tk k1 k2 = compareKey (innerMap tk) (injectFor tk k1) (injectFor tk k2)
	where 	innerMap :: InjectKeys t k2 mp a -> mp a
		innerMap = undefined

--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance (Eq (mp a)) => Eq (InjectKeys t k2 mp a) where
 (InjectKeys  kmp1) == (InjectKeys  kmp2) = (kmp1 == kmp2)

---------
-- Ord --
---------
instance (Ord (mp a)) => Ord (InjectKeys t k2 mp a) where
 compare (InjectKeys  kmp1) (InjectKeys  kmp2) = compare kmp1 kmp2

-- Show and read instances require transforming keys. Not hard but no time right now.
-- ----------
-- -- Show --
-- ----------
-- instance (Show (mp a)) => Show (InjectKeys t k2 mp a) where
--   showsPrec d (InjectKeys  mp)  = showsPrec d mp
-- 
-- ----------
-- -- Read --
-- ----------
-- instance (Read (mp a)) => R.Read (InjectKeys t k2 mp a) where
--  readPrec = InjectKeys  `fmap` R.readPrec
--  readListPrec = (L.map InjectKeys ) `fmap` R.readListPrec

------------------------
-- Typeable/Typeable1 --
------------------------
instance (Typeable1 mp) => Typeable1 (InjectKeys t k2 mp) where
 typeOf1 m = mkTyConApp (mkTyCon "Data.GMap.InjectKeys.InjectKeys") [typeOf1 innermp]
  where InjectKeys  innermp = m -- This is just to get the type for innermp!!
--------------
instance (Typeable1 (InjectKeys t k2 mp), Typeable a) => Typeable (InjectKeys t k2 mp a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance (Map mp) => Functor (InjectKeys t k2 mp) where
-- fmap :: (a -> b) -> EitherMap mapL mapR a -> EitherMap mapL mapR b
   fmap = mapInjectKeys  -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (Map mp, M.Monoid a) => M.Monoid (InjectKeys t k2 mp a) where
-- mempty :: EitherMap mapL mapR a
   mempty = emptyInjectKeys 
-- mappend :: EitherMap mapL mapR a -> EitherMap mapL mapR a -> EitherMap mapL mapR a
   mappend map0 map1 = unionInjectKeys  M.mappend map0 map1
-- mconcat :: [EitherMap mapL mapR a] -> EitherMap mapL mapR a
   mconcat maps = L.foldr (unionInjectKeys  M.mappend) emptyInjectKeys  maps

-------------------
-- Data.Foldable --
-------------------
instance (Map mp) => F.Foldable (InjectKeys t k2 mp) where
-- fold :: Monoid m => InjectKeys  mapL mapR m -> m
   fold mp = foldElemsInjectKeys  M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> InjectKeys  mapL mapR a -> m
   foldMap f mp = foldElemsInjectKeys  (\a b -> M.mappend (f a) b) M.mempty mp
-- fold :: (a -> b -> b) -> b -> InjectKeys  mapL mapR a -> b
   foldr f b0 mp = foldElemsInjectKeys  f b0 mp
-- foldl :: (a -> b -> a) -> a -> InjectKeys  mapL mapR b -> a
   foldl f b0 mp = foldElemsInjectKeys  (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- fold1 :: (a -> a -> a) -> InjectKeys  mapL mapR a -> a
   fold1 = undefined
-- foldl1 :: (a -> a -> a) -> InjectKeys  mapL mapR a -> a
   foldl1 = undefined
-}