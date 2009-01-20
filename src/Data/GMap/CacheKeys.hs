{-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction -fno-warn-orphans -fno-warn-unused-imports -fallow-undecidable-instances -Wall -fno-warn-missing-signatures #-}

module Data.GMap.CacheKeys
(-- * CacheKeys type
 CacheKeys
,cacheKeys
,uncacheKeys
) where

import Prelude hiding (foldr,map,filter,lookup)
import Data.GMap

import qualified Data.Monoid as M (Monoid(..))
import qualified Data.Foldable as F (Foldable(..))
import Data.Typeable
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import qualified Data.List as L

import GHC.Base hiding (map)
import qualified Text.Read as R 

-- | A map transformer that causes keys to be cached alongside elements
data CacheKeys mp a = CacheKeys !(mp (Key mp,a))

instance (Map mp) => Map (CacheKeys mp) where
	type Key (CacheKeys mp) = Key mp

	empty                 	= emptyCacheKeys
	singleton             	= singletonCacheKeys
	pair                  	= pairCacheKeys
	nonEmpty              	= nonEmptyCacheKeys
	status                	= statusCacheKeys
	addSize               	= addSizeCacheKeys
	lookup                	= lookupCacheKeys
	lookupCont              = lookupContCacheKeys
	alter			= alterCacheKeys
	insertWith            	= insertWithCacheKeys 
	insertWith'           	= insertWithCacheKeys'
	insertMaybe           	= insertMaybeCacheKeys
	fromAssocsWith		= fromAssocsWithCacheKeys
	fromAssocsMaybe 	= fromAssocsMaybeCacheKeys
	delete                	= deleteCacheKeys 
	adjustWith           	= adjustWithCacheKeys
	adjustWith' 		= adjustWithCacheKeys'
	adjustMaybe		= adjustMaybeCacheKeys
	venn			= vennCacheKeys
	venn'			= vennCacheKeys'
	vennMaybe		= vennMaybeCacheKeys
	union                 	= unionCacheKeys
	union'                	= unionCacheKeys'
	unionMaybe            	= unionMaybeCacheKeys
	disjointUnion		= disjointUnionCacheKeys
	intersection          	= intersectionCacheKeys
	intersection'         	= intersectionCacheKeys'
	intersectionMaybe     	= intersectionMaybeCacheKeys
	difference            	= differenceCacheKeys
	differenceMaybe       	= differenceMaybeCacheKeys
	isSubsetOf            	= isSubsetOfCacheKeys
	isSubmapOf             = isSubmapOfCacheKeys
	map                   	= mapCacheKeys
	map'                  	= mapCacheKeys'
	mapMaybe              	= mapMaybeCacheKeys
	mapWithKey            	= mapWithKeyCacheKeys
	mapWithKey'           	= mapWithKeyCacheKeys'
	filter                	= filterCacheKeys
	foldKeys		= foldKeysCacheKeys
	foldElems 		= foldElemsCacheKeys
	foldAssocs		= foldAssocsCacheKeys
	foldKeys'		= foldKeysCacheKeys'
	foldElems' 		= foldElemsCacheKeys'
	foldAssocs'		= foldAssocsCacheKeys'
	foldElemsUInt         	= foldElemsUIntCacheKeys
	valid                 	= validCacheKeys
 
instance (OrderedMap mp) => OrderedMap (CacheKeys mp) where
	compareKey 	= compareKeyCacheKeys
	fromAssocsAscWith = fromAssocsAscWithCacheKeys
	fromAssocsDescWith = fromAssocsDescWithCacheKeys
	fromAssocsAscMaybe = fromAssocsAscMaybeCacheKeys
	fromAssocsDescMaybe = fromAssocsDescMaybeCacheKeys
	foldElemsAsc	= foldElemsAscCacheKeys
	foldElemsDesc	= foldElemsDescCacheKeys
	foldKeysAsc	= foldKeysAscCacheKeys
	foldKeysDesc	= foldKeysDescCacheKeys
	foldAssocsAsc	= foldAssocsAscCacheKeys
	foldAssocsDesc	= foldAssocsDescCacheKeys
	foldElemsAsc'	= foldElemsAscCacheKeys'
	foldElemsDesc'	= foldElemsDescCacheKeys'
	foldKeysAsc'	= foldKeysAscCacheKeys'
	foldKeysDesc'	= foldKeysDescCacheKeys'
	foldAssocsAsc'	= foldAssocsAscCacheKeys'
	foldAssocsDesc'	= foldAssocsDescCacheKeys'
	
cacheKeys :: Map mp => mp a -> CacheKeys mp a
cacheKeys mp = CacheKeys (mapWithKey' (,) mp)

uncacheKeys :: Map mp => CacheKeys mp a -> mp a
uncacheKeys (CacheKeys mp) = map' snd mp

on :: (c -> d) -> (a -> b -> c) -> a -> b -> d
on f g a b = f $ g a b
	
emptyCacheKeys = CacheKeys empty

singletonCacheKeys k a = CacheKeys (singleton k (k,a))

pairCacheKeys k1 k2 = (cacheKeys `on`) `fmap` (pair k1 k2)

nonEmptyCacheKeys (CacheKeys kmp) = CacheKeys `fmap` (nonEmpty kmp)

statusCacheKeys (CacheKeys kmp) = 
	case (status kmp) of
		None -> None
		One k (_,a) -> One k a
		Many -> Many

addSizeCacheKeys (CacheKeys kmp) = addSize kmp

lookupCacheKeys k (CacheKeys kmp) = snd `fmap` (lookup k kmp)

lookupContCacheKeys f k (CacheKeys kmp) = lookupCont (f . snd) k kmp

withKey f (k,a) = let a' = f a in a' `seq` (k,a')
withKeyMaybe f (k,a) = do
	a' <- f a
	return (a' `seq` (k,a'))
withMaybeKeyMaybe f k mka = (\a' -> (k,a')) `fmap` (f (snd `fmap` mka))

alterCacheKeys f k (CacheKeys kmp) = CacheKeys (alter (withMaybeKeyMaybe f k) k kmp)

insertWithCacheKeys  f k a (CacheKeys kmp) = CacheKeys (insertWith  (withKey f) k (k,a) kmp)
insertWithCacheKeys' f k a (CacheKeys kmp) = CacheKeys (insertWith' (withKey f) k (a `seq` (k,a)) kmp)
insertMaybeCacheKeys f k a (CacheKeys kmp) = CacheKeys (insertMaybe (withKeyMaybe f) k (k,a) kmp)

deleteCacheKeys k (CacheKeys kmp) = CacheKeys (delete k kmp)

adjustWithCacheKeys  f k (CacheKeys kmp) = CacheKeys (adjustWith  (withKey f) k kmp)
adjustWithCacheKeys' f k (CacheKeys kmp) = CacheKeys (adjustWith' (withKey f) k kmp)
adjustMaybeCacheKeys f k (CacheKeys kmp) = CacheKeys (adjustMaybe (withKeyMaybe f) k kmp)

withKey2 f (k,a1) (_,a2) = let a' = f a1 a2 in a' `seq` (k,f a1 a2)
withKeyMaybe2 f (k,a1) (_,a2) = (\ a -> a `seq` (k,a)) `fmap` (f a1 a2)

vennCacheKeys  f (CacheKeys kmp1) (CacheKeys kmp2) = (CacheKeys leftDiff, CacheKeys inter, CacheKeys rightDiff)
	where (leftDiff,inter,rightDiff) = venn  (withKey2 f) kmp1 kmp2

vennCacheKeys' f (CacheKeys kmp1) (CacheKeys kmp2) = (CacheKeys leftDiff, CacheKeys inter, CacheKeys rightDiff)
	where (leftDiff,inter,rightDiff) = venn' (withKey2 f) kmp1 kmp2
	
vennMaybeCacheKeys f (CacheKeys kmp1) (CacheKeys kmp2) = (CacheKeys leftDiff, CacheKeys inter, CacheKeys rightDiff)
	where (leftDiff,inter,rightDiff) = vennMaybe (withKeyMaybe2 f) kmp1 kmp2

unionCacheKeys  f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (union  (withKey2 f) kmp1 kmp2)
unionCacheKeys' f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (union' (withKey2 f) kmp1 kmp2)
unionMaybeCacheKeys f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (unionMaybe (withKeyMaybe2 f) kmp1 kmp2)
disjointUnionCacheKeys (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (disjointUnion kmp1 kmp2)

intersectionCacheKeys  f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (intersection  (withKey2 f) kmp1 kmp2)
intersectionCacheKeys' f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (intersection' (withKey2 f) kmp1 kmp2)
intersectionMaybeCacheKeys f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (intersectionMaybe (withKeyMaybe2 f) kmp1 kmp2)

differenceCacheKeys (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (difference kmp1 kmp2)
differenceMaybeCacheKeys f (CacheKeys kmp1) (CacheKeys kmp2) = CacheKeys (differenceMaybe (withKeyMaybe2 f) kmp1 kmp2)

onAssoc f (_,a) = f a
onAssoc2 f (_,a) (_,b) = f a b

isSubsetOfCacheKeys   (CacheKeys kmp1) (CacheKeys kmp2) = isSubsetOf kmp1 kmp2
isSubmapOfCacheKeys f (CacheKeys kmp1) (CacheKeys kmp2) = isSubmapOf (onAssoc2 f) kmp1 kmp2

mapCacheKeys  f (CacheKeys kmp) = CacheKeys (map  (withKey f) kmp)
mapCacheKeys' f (CacheKeys kmp) = CacheKeys (map' (withKey f) kmp)
mapMaybeCacheKeys f (CacheKeys kmp) = CacheKeys (mapMaybe (withKeyMaybe f) kmp)
mapWithKeyCacheKeys  f (CacheKeys kmp) = CacheKeys (map  (\(k,a) -> (k,f k a)) kmp)
mapWithKeyCacheKeys' f (CacheKeys kmp) = CacheKeys (map' (\(k,a) -> let a' = f k a in a' `seq` (k,a')) kmp)

filterCacheKeys f (CacheKeys kmp) = CacheKeys (filter (onAssoc f) kmp)

foldElemsUIntCacheKeys f b (CacheKeys kmp) = foldElemsUInt  (onAssoc f) b kmp

validCacheKeys (CacheKeys kmp) = valid kmp

compareKeyCacheKeys cachemp k1 k2 = compareKey (innermp cachemp) k1 k2
	where 	innermp :: CacheKeys mp a -> mp a
		innermp _ = undefined

fromAssocsWithCacheKeys      f kas = CacheKeys (fromAssocsWith      (withKey2 f)      [(k,(k,a)) | (k,a) <- kas])
fromAssocsMaybeCacheKeys     f kas = CacheKeys (fromAssocsMaybe     (withKeyMaybe2 f) [(k,(k,a)) | (k,a) <- kas])
fromAssocsAscWithCacheKeys   f kas = CacheKeys (fromAssocsAscWith   (withKey2 f)      [(k,(k,a)) | (k,a) <- kas])
fromAssocsDescWithCacheKeys  f kas = CacheKeys (fromAssocsDescWith  (withKey2 f)      [(k,(k,a)) | (k,a) <- kas])
fromAssocsAscMaybeCacheKeys  f kas = CacheKeys (fromAssocsAscMaybe  (withKeyMaybe2 f) [(k,(k,a)) | (k,a) <- kas])
fromAssocsDescMaybeCacheKeys f kas = CacheKeys (fromAssocsDescMaybe (withKeyMaybe2 f) [(k,(k,a)) | (k,a) <- kas])

foldKeysCacheKeys      f b (CacheKeys kmp) = foldKeys      f b kmp
foldKeysCacheKeys'     f b (CacheKeys kmp) = foldKeys'     f b kmp
foldKeysAscCacheKeys   f b (CacheKeys kmp) = foldKeysAsc   f b kmp
foldKeysDescCacheKeys  f b (CacheKeys kmp) = foldKeysDesc  f b kmp
foldKeysAscCacheKeys'  f b (CacheKeys kmp) = foldKeysAsc'  f b kmp
foldKeysDescCacheKeys' f b (CacheKeys kmp) = foldKeysDesc' f b kmp

foldElemsCacheKeys  f b (CacheKeys kmp) = foldElems  (onAssoc f) b kmp
foldElemsCacheKeys' f b (CacheKeys kmp) = foldElems' (onAssoc f) b kmp
foldElemsAscCacheKeys   f b (CacheKeys kmp) = foldElemsAsc   (onAssoc f) b kmp
foldElemsDescCacheKeys  f b (CacheKeys kmp) = foldElemsDesc  (onAssoc f) b kmp
foldElemsAscCacheKeys'  f b (CacheKeys kmp) = foldElemsAsc'  (onAssoc f) b kmp
foldElemsDescCacheKeys' f b (CacheKeys kmp) = foldElemsDesc' (onAssoc f) b kmp

foldAssocsCacheKeys  f b (CacheKeys kmp) = foldElems  (uncurry f) b kmp
foldAssocsCacheKeys' f b (CacheKeys kmp) = foldElems' (uncurry f) b kmp
foldAssocsAscCacheKeys   f b (CacheKeys kmp) = foldElemsAsc   (uncurry f) b kmp
foldAssocsDescCacheKeys  f b (CacheKeys kmp) = foldElemsDesc  (uncurry f) b kmp
foldAssocsAscCacheKeys'  f b (CacheKeys kmp) = foldElemsAsc'  (uncurry f) b kmp
foldAssocsDescCacheKeys' f b (CacheKeys kmp) = foldElemsDesc' (uncurry f) b kmp

--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance (Eq (mp (Key mp,a))) => Eq (CacheKeys mp a) where
 (CacheKeys kmp1) == (CacheKeys kmp2) = (kmp1 == kmp2)

---------
-- Ord --
---------
instance (Ord (mp (Key mp,a))) => Ord (CacheKeys mp a) where
 compare (CacheKeys kmp1) (CacheKeys kmp2) = compare kmp1 kmp2

----------
-- Show --
----------
instance (Show (Key mp), Show a, Map mp) => Show (CacheKeys mp a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocs " . shows (assocs mp)

----------
-- Read --
----------
instance (Read (Key mp), Read a, Map mp) => R.Read (CacheKeys mp a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocs" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocs xs)
 readListPrec = R.readListPrecDefault

------------------------
-- Typeable/Typeable1 --
------------------------
instance (Typeable1 mp) => Typeable1 (CacheKeys mp) where
 typeOf1 m = mkTyConApp (mkTyCon "Data.GMap.CacheKeys.CacheKeys") [typeOf1 innermp]
  where CacheKeys innermp = m -- This is just to get the type for innermp!!
--------------
instance (Typeable1 (CacheKeys mp), Typeable a) => Typeable (CacheKeys mp a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance (Map mp) => Functor (CacheKeys mp) where
-- fmap :: (a -> b) -> EitherMap mapL mapR a -> EitherMap mapL mapR b
   fmap = mapCacheKeys -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (Map mp, M.Monoid a) => M.Monoid (CacheKeys mp a) where
-- mempty :: EitherMap mapL mapR a
   mempty = emptyCacheKeys
-- mappend :: EitherMap mapL mapR a -> EitherMap mapL mapR a -> EitherMap mapL mapR a
   mappend map0 map1 = unionCacheKeys M.mappend map0 map1
-- mconcat :: [EitherMap mapL mapR a] -> EitherMap mapL mapR a
   mconcat maps = L.foldr (unionCacheKeys M.mappend) emptyCacheKeys maps

-------------------
-- Data.Foldable --
-------------------
instance (Map mp) => F.Foldable (CacheKeys mp) where
-- fold :: Monoid m => CacheKeys mapL mapR m -> m
   fold mp = foldElemsCacheKeys M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> CacheKeys mapL mapR a -> m
   foldMap f mp = foldElemsCacheKeys (\a b -> M.mappend (f a) b) M.mempty mp
-- fold :: (a -> b -> b) -> b -> CacheKeys mapL mapR a -> b
   foldr f b0 mp = foldElemsCacheKeys f b0 mp
-- foldl :: (a -> b -> a) -> a -> CacheKeys mapL mapR b -> a
   foldl f b0 mp = foldElemsCacheKeys (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- fold1 :: (a -> a -> a) -> CacheKeys mapL mapR a -> a
   fold1 = undefined
-- foldl1 :: (a -> a -> a) -> CacheKeys mapL mapR a -> a
   foldl1 = undefined
-}

