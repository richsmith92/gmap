{-# OPTIONS_GHC -fglasgow-exts -XNoMonomorphismRestriction #-}

module Test.GMap where

import Test.QuickCheck
import Test.QuickCheck.Batch(bottom,isBottom)
import Test.GMap.Utils

import Data.GMap as G
import Data.GMap.AssocList
-- import Data.GMap.ListMap
import Data.GMap.UnitMap
import Data.GMap.MaybeMap
import Data.GMap.EitherMap
import Data.GMap.OrdMap
import Data.GMap.IntMap
-- import Data.GMap.SerialMap
import Data.GMap.CacheKeys
import Data.GMap.TupleMap
import Data.GMap.EnumMap
import Data.GMap.ChoiceMap
-- import Data.GMap.BitMap
import Data.GMap.InjectKeys

-- import Data.Serial
-- import Data.Serial.Buildable.WordList()

import qualified Data.List as L
import Prelude hiding (map,lookup)

import Control.Monad(liftM)
import Data.Maybe
import Data.Ord
import qualified Data.List as L

import System.IO
import System.Environment

import GHC.Base hiding (map)

mapSortKeys :: OrderedMap map => map a -> [Key map] -> [Key map]
mapSortKeys mp = L.sortBy (compareKey mp)

mapSortAssocs :: OrderedMap map => map a -> [(Key map,a)] -> [(Key map,a)]
mapSortAssocs mp = L.sortBy (\ (k1,_) (k2,_) -> compareKey mp k1 k2)

-- ### Testing OrderedMap methods ###

prop_lookup_empty mp k =
	Nothing == (lookup k $ empty `like` mp)

prop_lookup_singleton mp (k,a) =
	Just a == (lookup k $ singleton k a `like` mp)

-- General test pattern
doWith k a mp f = lookup k $ f $ insert k a mp

-- Another useful pattern
doWithout k mp f = lookup k $ f $ delete k mp

prop_insert_with mp (k,a) =
	Just a == (doWith k a mp $ insert k a)

prop_insert_without mp (k,a) =
	Just a == (doWithout k mp $ insert k a)

prop_insertWith_with mp (k,a1,a2,f) =
	Just (f a1) == (doWith k a1 mp $ insertWith f k a2)

prop_insertWith_without mp (k,a2,f) =
	Just a2 == (doWithout k mp $ insertWith f k a2)

prop_insertWith'_with mp (k,a1,a2,f) =
	Just (f a1) == (doWith k a1 mp $ insertWith' f k a2)

prop_insertWith'_without mp (k,a2,f) =
	Just a2 == (doWithout k mp $ insertWith' f k a2)

prop_insertMaybe_with mp (k,a1,a2,f) =
	(f =<< Just a1) == (doWith k a1 mp $ insertMaybe f k a2)

prop_insertMaybe_without mp (k,a2,f) =
	Just a2 == (doWithout k mp $ insertMaybe f k a2)

prop_insertMaybe'_with mp (k,a1,a2,f) =
	(f =<< Just a1) == (doWith k a1 mp $ insertMaybe' f k a2)

prop_insertMaybe'_without mp (k,a2,f) =
	Just a2 == (doWithout k mp $ insertMaybe' f k a2)

-- Dont test insertAssocs yet, still not sure whether to include them

prop_delete_with mp (k,a) =
	Nothing == (doWith k a mp $ delete k)

prop_delete_without mp k =
	Nothing == (doWithout k mp $ delete k)

prop_adjustWith_with mp (k,a,f) =
	Just (f a) == (doWith k a mp $ adjustWith f k)

prop_adjustWith_without mp (k,f) =
	Nothing == (doWithout k mp $ adjustWith f k)

prop_adjustWith'_with mp (k,a,f) =
	Just (f a) == (doWith k a mp $ adjustWith' f k)

prop_adjustWith'_without mp (k,f) =
	Nothing == (doWithout k mp $ adjustWith' f k)

prop_adjustMaybe_with mp (k,a,f) =
	(f =<< Just a) == (doWith k a mp $ adjustMaybe f k)

prop_adjustMaybe_without mp (k,f) =
	Nothing == (doWithout k mp $ adjustMaybe f k)

prop_adjustMaybe'_with mp (k,a,f) =
	(f =<< Just a) == (doWith k a mp $ adjustMaybe' f k)

prop_adjustMaybe'_without mp (k,f) =
	Nothing == (doWithout k mp $ adjustMaybe' f k)

-- The various merges are better tested by the comparison tests

prop_isSubsetOf mp as =
	isSubsetOf mp (insertAssocs as mp)

prop_isSubmapOf mp (f,as) =
	isSubmapOf (\ a b -> f a == b) mp ((map f $ insertAssocsWith const as mp) `like` mp)

prop_map mp (k,a,f) =
	Just (f a) == (doWith k a mp $ \ mp -> map f mp `like` mp)

prop_map' mp (k,a,f) =
	Just (f a) == (doWith k a mp $ \ mp -> map' f mp `like` mp)

prop_mapMaybe mp (k,a,f) =
	(f =<< Just a) == (doWith k a mp $ \ mp -> G.mapMaybe f mp `like` mp)

prop_mapMaybe' mp (k,a,f) =
	(f =<< Just a) == (doWith k a mp $ \ mp -> G.mapMaybe' f mp `like` mp)

prop_mapWithKey mp (k,a,f) =
	Just (f k a) == (doWith k a mp $ \ mp -> mapWithKey f mp `like` mp)

prop_mapWithKey' mp (k,a,f) =
	Just (f k a) == (doWith k a mp $ \ mp -> mapWithKey' f mp `like` mp)

prop_filter_in mp (k,a) =
	Just a == (doWith k a mp $ G.filter (a ==))

prop_filter_out mp (k,a) =
	Nothing == (doWith k a mp $ G.filter (a /=))

-- Dont yet know how to test folds. Need to randomly produce an associative function (or use const and lookup?)

prop_valid mp () =
	Nothing == valid mp

-- ### Strictness tests for OrderedMap ###
-- For lazy funs make every resulting elem bottom
-- For strict funs make a single resulting elem bottom

isMaybeBottom a =
	(not $ isBottom a) &&
	case a of
		Nothing -> True
		Just a' -> isBottom a'

isLazyAlter mp k f =
	let 	mp' = f mp `like` mp
	in	(not $ isBottom mp') &&
		(isMaybeBottom $ lookup k mp')

isStrictAlter mp k f =
	let 	mp' = f mp `like` mp
	in	isBottom mp'

prop_lazy_alter mp k =
	isLazyAlter mp k $ alter (\a -> Just bottom) k

prop_strict_alter' mp k =
	isStrictAlter mp k $ alter' (\a -> Just bottom) k

prop_lazy_insertWith mp k =
	isLazyAlter mp k $ insertWith (\a -> bottom) k bottom

-- insertWith' is currently only strict if the key already exists
-- !!! Remember to change this test if the semantics of insertWith' are changed
prop_strict_insertWith' mp (k,a) =
	isStrictAlter (insert k a mp) k $ insertWith' (\a -> bottom) k bottom

prop_lazy_insertMaybe mp k =
	isLazyAlter mp k $ insertMaybe (\a -> Just bottom) k bottom

-- insertMaybe' is currently only strict if the key already exists
-- !!! Remember to change this test if the semantics of insertMaybe' are changed
prop_strict_insertMaybe' mp (k,a) =
	isStrictAlter (insert k a mp) k $ insertMaybe' (\a -> Just bottom) k bottom

-- For adjusts we need to ensure that k is in the map
prop_lazy_adjustWith mp (k,a) =
	isLazyAlter (insert k a mp) k $ adjustWith (\a -> bottom) k

prop_strict_adjustWith' mp (k,a) =
	isStrictAlter (insert k a mp) k $ adjustWith' (\a -> bottom) k

prop_lazy_adjustMaybe mp (k,a) =
	isLazyAlter (insert k a mp) k $ adjustMaybe (\a -> Just bottom) k

prop_strict_adjustMaybe' mp (k,a) =
	isStrictAlter (insert k a mp) k $ adjustMaybe' (\a -> Just bottom) k

isLazyMerge :: OrderedMap map => map a -> map a -> Key map -> (map a -> map a -> map a) -> Bool
isLazyMerge mp1 mp2 k f =
	let 	mp' = f mp1 mp2 `like` mp1
	in	(not $ isBottom mp') &&
		(isMaybeBottom $ lookup k mp')

isStrictMerge :: OrderedMap map => map a -> map a -> Key map -> (map a -> map a -> map a) -> Bool
isStrictMerge mp1 mp2 k f =
	let 	mp' = f mp1 mp2 `like` mp1
	in	isBottom mp'

sel1 (a,b,c) = a
sel2 (a,b,c) = b
sel3 (a,b,c) = c

-- For merge tests need to ensure that resulting map has at least one assoc or the tests dont work
-- Many of these tests need to have a shared key in both maps.

prop2_lazy_venn_left (mp1,mp2) (k) =
	isLazyMerge (map (const bottom) (insert k bottom mp1)) (delete k mp2) k $ (sel1 `on` venn const)

prop2_lazy_venn_inter (mp1,mp2) (k,a) =
	isLazyMerge (insert k a mp1) (insert k a mp2) k $ (sel2 `on` venn (\a b -> bottom))

prop2_lazy_venn_right (mp1,mp2) (k) =
	isLazyMerge (delete k mp1) (map (const bottom) (insert k bottom mp2)) k $ (sel3 `on` venn const)

prop2_strict_venn'_inter (mp1,mp2) (k,a) =
	isStrictMerge (insert k bottom mp1) (insert k a mp2) k $ (sel2 `on` venn' const)

prop2_lazy_union (mp1,mp2) (k,a) =
	isLazyMerge (insert k a mp1) (insert k a mp2) k $ union (\a b -> bottom)

prop2_strict_union' (mp1,mp2) (k,a) =
	isStrictMerge (insert k a mp1) (insert k bottom mp2) k $ union' (\a b -> a `seq` b `seq` a)

prop2_lazy_unionMaybe (mp1,mp2) (k,a) =
	isLazyMerge (insert k a mp1) (insert k a mp2) k $ unionMaybe (\a b -> Just bottom)

prop2_strict_unionMaybe' (mp1,mp2) (k,a) =
	isStrictMerge (insert k a mp1) (insert k bottom mp2) k $ unionMaybe' (\a b -> a `seq` b `seq` Just a)

prop2_lazy_intersection (mp1,mp2) (k,a) =
	isLazyMerge (insert k a mp1) (insert k a mp2) k $ intersection (\a b -> bottom)

prop2_strict_intersection' (mp1,mp2) (k,a) =
	isStrictMerge (insert k a mp1) (insert k bottom mp2) k $ intersection' (\a b -> a `seq` b `seq` a)

prop2_lazy_intersectionMaybe (mp1,mp2) (k,a) =
	isLazyMerge (insert k a mp1) (insert k a mp2) k $ intersectionMaybe (\a b -> Just bottom)

prop2_strict_intersectionMaybe' (mp1,mp2) (k,a) =
	isStrictMerge (insert k a mp1) (insert k bottom mp2) k $ intersectionMaybe' (\a b -> a `seq` b `seq` Just a)

prop2_lazy_differenceMaybe (mp1,mp2) (k,a) =
	isLazyMerge (insert k a mp1) (insert k a mp2) k $ differenceMaybe (\a b -> Just bottom)

prop2_strict_differenceMaybe' (mp1,mp2) (k,a) =
	isStrictMerge (insert k a mp1) (insert k bottom mp2) k $ differenceMaybe' (\a b -> a `seq` b `seq` Just a)

-- Need to have a nonEmpty OrderedMap to test strictness of map
prop_lazy_map mp (k,a) =
	isLazyAlter (insert k a mp) k $ map (\ a' -> bottom)

prop_strict_map' mp (k,a) =
	isStrictAlter (insert k a mp) k $ map' (\ a' -> if (a==a') then bottom else a')

prop_lazy_mapMaybe mp (k,a) =
	isLazyAlter (insert k a mp) k $ G.mapMaybe (\ a' -> Just bottom)

prop_strict_mapMaybe' mp (k,a) =
	isStrictAlter (insert k a mp) k $ G.mapMaybe' (\ a' -> if (a==a') then (Just bottom) else (Just a'))

prop_lazy_mapWithKey mp (k,a) =
	isLazyAlter (insert k a mp) k $ mapWithKey (\ k' a' -> bottom)

prop_strict_mapWithKey' mp (k,a) =
	isStrictAlter (insert k a mp) k $ mapWithKey' (\ k' a' -> if ((k',a')==(k,a)) then bottom else a')

-- Lazy and strict folds are identical if the map has zero or one assocs so we must ensure that they have at least two assocs
-- We test folds by ensuring that the first accumalated value is bottom and the rest are Justs.

foldArg a b
	| isBottom b 	= Just a
	| isNothing b	= bottom
	| otherwise	= Just a

foldArgK _ = foldArg

prop_lazy_foldKeys mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldKeys foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

prop_strict_foldKeys' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldKeys' foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

prop_lazy_foldElems mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldElems foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

prop_strict_foldElems' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldElems' foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

prop_lazy_foldAssocs mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldAssocs foldArgK Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

prop_strict_foldAssocs' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldAssocs' foldArgK Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

-- ### Comparisons to AList ###

comp_empty mp () =
	assocsAsc (empty `like` mp)

comp_singleton mp (k,a) =
	assocsAsc (singleton k a `like` mp)

comp_pair mp (k1,k2,a1,a2) =
	fmap assocsAsc ((fmap (\ f -> f a1 a2) (pair k1 k2)) `like` (Just mp))

comp_status mp () =
	status mp

comp_nonEmpty mp () =
	fmap assocsAsc $ nonEmpty mp

comp_addSize mp (I# i) =
	I# (addSize mp i)

comp_lookup mp k =
	lookup k mp

comp_lookupCont mp (k,f) =
	lookupCont f k mp `likeMaybeElem` mp

comp_alter mp (k,f) =
	assocsAsc $ alter f k mp

comp_alter' mp (k,f) =
	assocsAsc $ alter' f k mp

comp_insertWith mp (k,a,f) =
	assocsAsc $ insertWith f k a mp

comp_insertWith' mp (k,a,f) =
	assocsAsc $ insertWith' f k a mp

-- comp_insertAssocsWith : Waiting on updates to OrderedMap api
-- comp_insertAssocsMaybe

comp_insertMaybe mp (k,a,f) =
	assocsAsc $ insertMaybe f k a mp

comp_insertMaybe' mp (k,a,f) =
	assocsAsc $ insertMaybe' f k a mp

comp_delete mp k =
	assocsAsc $ delete k mp

comp_adjustWith mp (k,f) =
	assocsAsc $ adjustWith f k mp

comp_adjustWith' mp (k,f) =
	assocsAsc $ adjustWith' f k mp

comp_adjustMaybe mp (k,f) =
	assocsAsc $ adjustMaybe f k mp

comp_adjustMaybe' mp (k,f) =
	assocsAsc $ adjustMaybe' f k mp

-- Why dont tuple functors work properly?
-- Note that the type is more constrained than venn.
vennAssocs :: (OrderedMap map, Ord (Key map)) => (map a, map a, map a) -> ([(Key map,a)],[(Key map,a)],[(Key map,a)])
vennAssocs (mpa,mpc,mpb) = (assocsAsc mpa,assocsAsc mpc,assocsAsc mpb)

comp2_venn (mp1,mp2) f =
	vennAssocs $ venn f mp1 mp2

comp2_venn' (mp1,mp2) f =
	vennAssocs $ venn' f mp1 mp2

comp2_vennMaybe (mp1,mp2) f =
	vennAssocs $ vennMaybe f mp1 mp2

-- Use venn to obtain disjoint maps - so relies on venn being correct
comp2_disjointUnion (mp1,mp2) () =
	assocsAsc $ disjointUnion left right `like` mp1 `like` mp2
	where	(left,_,right) = venn const mp1 mp2

comp2_union (mp1,mp2) f =
	assocsAsc $ union f mp1 mp2 `like` mp1 `like` mp2

comp2_union' (mp1,mp2) f =
	assocsAsc $ union' f mp1 mp2 `like` mp1 `like` mp2

comp2_unionMaybe (mp1,mp2) f =
	assocsAsc $ unionMaybe f mp1 mp2 `like` mp1 `like` mp2

comp2_unionMaybe' (mp1,mp2) f =
	assocsAsc $ unionMaybe' f mp1 mp2 `like` mp1 `like` mp2

comp2_intersection (mp1,mp2) f =
	assocsAsc $ intersection f mp1 mp2 `like` mp1 `like` mp2

comp2_intersection' (mp1,mp2) f =
	assocsAsc $ intersection' f mp1 mp2 `like` mp1 `like` mp2

comp2_intersectionMaybe (mp1,mp2) f =
	assocsAsc $ intersectionMaybe f mp1 mp2 `like` mp1 `like` mp2

comp2_intersectionMaybe' (mp1,mp2) f =
	assocsAsc $ intersectionMaybe' f mp1 mp2 `like` mp1 `like` mp2

comp2_difference (mp1,mp2) () =
	assocsAsc $ difference mp1 mp2 `like` mp1 `like` mp2

comp2_differenceMaybe (mp1,mp2) f =
	assocsAsc $ differenceMaybe f mp1 mp2 `like` mp1 `like` mp2

comp2_differenceMaybe' (mp1,mp2) f =
	assocsAsc $ differenceMaybe' f mp1 mp2 `like` mp1 `like` mp2

comp2_isSubsetOf (mp1,mp2) () =
	isSubsetOf mp1 mp2

comp2_isSubmapOf (mp1,mp2) f =
	isSubmapOf f mp1 mp2

comp_map mp f =
	assocsAsc $ G.map f mp `like` mp

comp_map' mp f =
	assocsAsc $ G.map' f mp `like` mp

comp_mapMaybe mp f =
	assocsAsc $ G.mapMaybe f mp `like` mp

comp_mapMaybe' mp f =
	assocsAsc $ G.mapMaybe' f mp `like` mp

comp_mapWithKey mp f =
	assocsAsc $ G.mapWithKey f mp `like` mp

comp_mapWithKey' mp f =
	assocsAsc $ G.mapWithKey' f mp `like` mp

comp_filter mp f =
	assocsAsc $ G.filter f mp

comp_insert mp (k,a) =
	assocsAsc $ insert k a mp

-- Dont compare folds because they depend on ordering

comp_size mp () =
	size mp

comp_insertAssocs mp as =
	assocsAsc $ insertAssocs as mp

comp_fromAssocs mp as =
	assocsAsc $ fromAssocs as `like` mp

comp_fromAssocsWith mp (f,as) =
	assocsAsc $ fromAssocsWith f as `like` mp

comp2_isProperSubsetOf (mp1,mp2) () =
	isProperSubsetOf mp1 mp2

comp2_isProperSubmapOfBy (mp1,mp2) f =
	isProperSubmapOfBy f mp1 mp2

-- comp_lookupM : Need to fix monad

comp_keys mp () =
	mapSortKeys mp $ keys mp

comp_elems mp () =
	mapSortKeys mp $ elems mp

comp_assocs mp () =
	assocsAsc mp

-- ### Testing OrderedMap methods ###

propO_keysAsc mp () =
	keysAsc mp == (L.map fst $ assocsAsc mp)

propO_keysDesc mp () =
	keysDesc mp == (L.map fst $ assocsDesc mp)

propO_elemsAsc mp () =
	elemsAsc mp == (L.map snd $ assocsAsc mp)

propO_elemsDesc mp () =
	elemsDesc mp == (L.map snd $ assocsDesc mp)

propO_assocsAsc mp () =
	let 	as = assocsAsc mp
	in	L.sortBy (\ (k1,_) (k2,_) -> compareKey mp k1 k2) as == as

propO_assocsDesc mp () =
	let 	as = assocsDesc mp
	in	L.sortBy (\ (k1,_) (k2,_) -> compareKey mp k2 k1) as == as

-- ### Strictness tests for OrderedMap ###

propO_lazy_foldKeysAsc mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldKeysAsc foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_strict_foldKeysAsc' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldKeysAsc' foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_lazy_foldKeysDesc mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldKeysDesc foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_strict_foldKeysDesc' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldKeysDesc' foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_lazy_foldElemsAsc mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldElemsAsc foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_strict_foldElemsAsc' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldElemsAsc' foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_lazy_foldElemsDesc mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldElemsDesc foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_strict_foldElemsDesc' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldElemsDesc' foldArg Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_lazy_foldAssocsAsc mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldAssocsAsc foldArgK Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_strict_foldAssocsAsc' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldAssocsAsc' foldArgK Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_lazy_foldAssocsDesc mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	not $ isBottom $ foldAssocsDesc foldArgK Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

propO_strict_foldAssocsDesc' mp ((k1,a1),(k2,a2)) =
	k1 /= k2 ==>
	isBottom $ foldAssocsDesc' foldArgK Nothing $ insertAssocs [(k1,a1),(k2,a2)] mp

keyedLike :: OrderedMap map => map a -> map b -> map a
keyedLike mp _ = mp

propO_nubAscWith mp as =
	(nubAscWith (empty `keyedLike` mp) as) == (mapSortKeys mp $ L.nub as)

propO_nubDescWith mp as =
	(nubDescWith (empty `keyedLike` mp) as) == (reverse $ mapSortKeys mp $ L.nub as)

propO_sortAscWith mp as =
	(sortAscWith (empty `keyedLike` mp) as) == (mapSortKeys mp as)

propO_sortDescWith mp as =
	(sortDescWith (empty `keyedLike` mp) as) == (reverse $ mapSortKeys mp as)

-- Most methods better tested by comparisons to SList

-- ### Comparisons to SList ###

-- comp_compareKey : Useless because of the newtyping required for SList

compO_fromAssocsAscWith mp (f,as) =
	assocsAsc $ fromAssocsAscWith f (mapSortAssocs mp as) `like` mp

compO_fromAssocsDescWith mp (f,as) =
	assocsAsc $ fromAssocsDescWith f (reverse $ mapSortAssocs mp as) `like` mp

compO_fromAssocsAscMaybe mp (f,as) =
	assocsAsc $ fromAssocsAscMaybe f (mapSortAssocs mp as) `like` mp

compO_fromAssocsDescMaybe mp (f,as) =
	assocsAsc $ fromAssocsDescMaybe f (reverse $ mapSortAssocs mp as) `like` mp

compO_insertAssocsAscWith mp (f,as) =
	assocsAsc $ insertAssocsAscWith f (mapSortAssocs mp as) mp

compO_insertAssocsDescWith mp (f,as) =
	assocsAsc $ insertAssocsDescWith f (reverse $ mapSortAssocs mp as) mp

compO_insertAssocsAscMaybe mp (f,as) =
	assocsAsc $ insertAssocsAscMaybe f (mapSortAssocs mp as) mp

compO_insertAssocsDescMaybe mp (f,as) =
	assocsAsc $ insertAssocsDescMaybe f (reverse $ mapSortAssocs mp as) mp

compO_foldElemsAsc mp (f,b) =
	foldElemsAsc f b mp `likeElem` mp

compO_foldElemsDesc mp (f,b) =
	foldElemsDesc f b mp `likeElem` mp

compO_foldElemsAsc' mp (f,b) =
	foldElemsAsc' f b mp `likeElem` mp

compO_foldElemsDesc' mp (f,b) =
	foldElemsDesc' f b mp `likeElem` mp

compO_foldKeysAsc mp (f,b) =
	foldKeysAsc f b mp `likeElem` mp

compO_foldKeysDesc mp (f,b) =
	foldKeysDesc f b mp `likeElem` mp

compO_foldKeysAsc' mp (f,b) =
	foldKeysAsc' f b mp `likeElem` mp

compO_foldKeysDesc' mp (f,b) =
	foldKeysDesc' f b mp `likeElem` mp

compO_foldAssocsAsc mp (f,b) =
	foldAssocsAsc f b mp `likeElem` mp

compO_foldAssocsDesc mp (f,b) =
	foldAssocsDesc f b mp `likeElem` mp

compO_foldAssocsAsc' mp (f,b) =
	foldAssocsAsc' f b mp `likeElem` mp

compO_foldAssocsDesc' mp (f,b) =
	foldAssocsDesc' f b mp `likeElem` mp

compO_elemsAsc mp () =
	elemsAsc mp

compO_elemsDesc mp () =
	elemsDesc mp

compO_keysAsc mp () =
	keysAsc mp

compO_keysDesc mp () =
	keysDesc mp

compO_assocsAsc mp () =
	assocsAsc mp

compO_assocsDesc mp () =
	assocsDesc mp

-- Partitions, sorts not yet implemented so not tested.

-- ### Testing OrdMap methods ###

-- prop_compareKey mp (k1,k2) =
-- 	compareKey mp k1 k2 == compare k1 k2

-- ### Scripts to collate tests ###

propList = testList "Test/GMap.hs" "prop_" "SimpleTest "
props = [(SimpleTest prop_lookup_empty,"prop_lookup_empty"),(SimpleTest prop_lookup_singleton,"prop_lookup_singleton"),(SimpleTest prop_insert_with,"prop_insert_with"),(SimpleTest prop_insert_without,"prop_insert_without"),(SimpleTest prop_insertWith_with,"prop_insertWith_with"),(SimpleTest prop_insertWith_without,"prop_insertWith_without"),(SimpleTest prop_insertWith'_with,"prop_insertWith'_with"),(SimpleTest prop_insertWith'_without,"prop_insertWith'_without"),(SimpleTest prop_insertMaybe_with,"prop_insertMaybe_with"),(SimpleTest prop_insertMaybe_without,"prop_insertMaybe_without"),(SimpleTest prop_insertMaybe'_with,"prop_insertMaybe'_with"),(SimpleTest prop_insertMaybe'_without,"prop_insertMaybe'_without"),(SimpleTest prop_delete_with,"prop_delete_with"),(SimpleTest prop_delete_without,"prop_delete_without"),(SimpleTest prop_adjustWith_with,"prop_adjustWith_with"),(SimpleTest prop_adjustWith_without,"prop_adjustWith_without"),(SimpleTest prop_adjustWith'_with,"prop_adjustWith'_with"),(SimpleTest prop_adjustWith'_without,"prop_adjustWith'_without"),(SimpleTest prop_adjustMaybe_with,"prop_adjustMaybe_with"),(SimpleTest prop_adjustMaybe_without,"prop_adjustMaybe_without"),(SimpleTest prop_adjustMaybe'_with,"prop_adjustMaybe'_with"),(SimpleTest prop_adjustMaybe'_without,"prop_adjustMaybe'_without"),(SimpleTest prop_isSubsetOf,"prop_isSubsetOf"),(SimpleTest prop_isSubmapOf,"prop_isSubmapOf"),(SimpleTest prop_map,"prop_map"),(SimpleTest prop_map',"prop_map'"),(SimpleTest prop_mapMaybe,"prop_mapMaybe"),(SimpleTest prop_mapMaybe',"prop_mapMaybe'"),(SimpleTest prop_mapWithKey,"prop_mapWithKey"),(SimpleTest prop_mapWithKey',"prop_mapWithKey'"),(SimpleTest prop_filter_in,"prop_filter_in"),(SimpleTest prop_filter_out,"prop_filter_out"),(SimpleTest prop_valid,"prop_valid"),(SimpleTest prop_lazy_alter,"prop_lazy_alter"),(SimpleTest prop_strict_alter',"prop_strict_alter'"),(SimpleTest prop_lazy_insertWith,"prop_lazy_insertWith"),(SimpleTest prop_strict_insertWith',"prop_strict_insertWith'"),(SimpleTest prop_lazy_insertMaybe,"prop_lazy_insertMaybe"),(SimpleTest prop_strict_insertMaybe',"prop_strict_insertMaybe'"),(SimpleTest prop_lazy_adjustWith,"prop_lazy_adjustWith"),(SimpleTest prop_strict_adjustWith',"prop_strict_adjustWith'"),(SimpleTest prop_lazy_adjustMaybe,"prop_lazy_adjustMaybe"),(SimpleTest prop_strict_adjustMaybe',"prop_strict_adjustMaybe'"),(SimpleTest prop_lazy_map,"prop_lazy_map"),(SimpleTest prop_strict_map',"prop_strict_map'"),(SimpleTest prop_lazy_mapMaybe,"prop_lazy_mapMaybe"),(SimpleTest prop_strict_mapMaybe',"prop_strict_mapMaybe'"),(SimpleTest prop_lazy_mapWithKey,"prop_lazy_mapWithKey"),(SimpleTest prop_strict_mapWithKey',"prop_strict_mapWithKey'"),(SimpleTest prop_lazy_foldKeys,"prop_lazy_foldKeys"),(SimpleTest prop_strict_foldKeys',"prop_strict_foldKeys'"),(SimpleTest prop_lazy_foldElems,"prop_lazy_foldElems"),(SimpleTest prop_strict_foldElems',"prop_strict_foldElems'"),(SimpleTest prop_lazy_foldAssocs,"prop_lazy_foldAssocs"),(SimpleTest prop_strict_foldAssocs',"prop_strict_foldAssocs'")]

compList = testList "Test/GMap.hs" "comp_" "compareTest "
comps = [(compareTest comp_empty,"comp_empty"),(compareTest comp_singleton,"comp_singleton"),(compareTest comp_pair,"comp_pair"),(compareTest comp_status,"comp_status"),(compareTest comp_nonEmpty,"comp_nonEmpty"),(compareTest comp_addSize,"comp_addSize"),(compareTest comp_lookup,"comp_lookup"),(compareTest comp_lookupCont,"comp_lookupCont"),(compareTest comp_alter,"comp_alter"),(compareTest comp_alter',"comp_alter'"),(compareTest comp_insertWith,"comp_insertWith"),(compareTest comp_insertWith',"comp_insertWith'"),(compareTest comp_insertMaybe,"comp_insertMaybe"),(compareTest comp_insertMaybe',"comp_insertMaybe'"),(compareTest comp_delete,"comp_delete"),(compareTest comp_adjustWith,"comp_adjustWith"),(compareTest comp_adjustWith',"comp_adjustWith'"),(compareTest comp_adjustMaybe,"comp_adjustMaybe"),(compareTest comp_adjustMaybe',"comp_adjustMaybe'"),(compareTest comp_map,"comp_map"),(compareTest comp_map',"comp_map'"),(compareTest comp_mapMaybe,"comp_mapMaybe"),(compareTest comp_mapMaybe',"comp_mapMaybe'"),(compareTest comp_mapWithKey,"comp_mapWithKey"),(compareTest comp_mapWithKey',"comp_mapWithKey'"),(compareTest comp_filter,"comp_filter"),(compareTest comp_insert,"comp_insert"),(compareTest comp_size,"comp_size"),(compareTest comp_insertAssocs,"comp_insertAssocs"),(compareTest comp_fromAssocs,"comp_fromAssocs"),(compareTest comp_fromAssocsWith,"comp_fromAssocsWith"),(compareTest comp_keys,"comp_keys"),(compareTest comp_elems,"comp_elems"),(compareTest comp_assocs,"comp_assocs")]

prop2List = testList "Test/GMap.hs" "prop2_" "SimpleTest2 "
prop2s = [(SimpleTest2 prop2_lazy_venn_left,"prop2_lazy_venn_left"),(SimpleTest2 prop2_lazy_venn_inter,"prop2_lazy_venn_inter"),(SimpleTest2 prop2_lazy_venn_right,"prop2_lazy_venn_right"),(SimpleTest2 prop2_strict_venn'_inter,"prop2_strict_venn'_inter"),(SimpleTest2 prop2_lazy_union,"prop2_lazy_union"),(SimpleTest2 prop2_strict_union',"prop2_strict_union'"),(SimpleTest2 prop2_lazy_unionMaybe,"prop2_lazy_unionMaybe"),(SimpleTest2 prop2_strict_unionMaybe',"prop2_strict_unionMaybe'"),(SimpleTest2 prop2_lazy_intersection,"prop2_lazy_intersection"),(SimpleTest2 prop2_strict_intersection',"prop2_strict_intersection'"),(SimpleTest2 prop2_lazy_intersectionMaybe,"prop2_lazy_intersectionMaybe"),(SimpleTest2 prop2_strict_intersectionMaybe',"prop2_strict_intersectionMaybe'"),(SimpleTest2 prop2_lazy_differenceMaybe,"prop2_lazy_differenceMaybe"),(SimpleTest2 prop2_strict_differenceMaybe',"prop2_strict_differenceMaybe'")]

comp2List = testList "Test/GMap.hs" "comp2_" "compareTest2 "
comp2s = [(compareTest2 comp2_venn,"comp2_venn"),(compareTest2 comp2_venn',"comp2_venn'"),(compareTest2 comp2_vennMaybe,"comp2_vennMaybe"),(compareTest2 comp2_disjointUnion,"comp2_disjointUnion"),(compareTest2 comp2_union,"comp2_union"),(compareTest2 comp2_union',"comp2_union'"),(compareTest2 comp2_unionMaybe,"comp2_unionMaybe"),(compareTest2 comp2_unionMaybe',"comp2_unionMaybe'"),(compareTest2 comp2_intersection,"comp2_intersection"),(compareTest2 comp2_intersection',"comp2_intersection'"),(compareTest2 comp2_intersectionMaybe,"comp2_intersectionMaybe"),(compareTest2 comp2_intersectionMaybe',"comp2_intersectionMaybe'"),(compareTest2 comp2_difference,"comp2_difference"),(compareTest2 comp2_differenceMaybe,"comp2_differenceMaybe"),(compareTest2 comp2_differenceMaybe',"comp2_differenceMaybe'"),(compareTest2 comp2_isSubsetOf,"comp2_isSubsetOf"),(compareTest2 comp2_isSubmapOf,"comp2_isSubmapOf"),(compareTest2 comp2_isProperSubsetOf,"comp2_isProperSubsetOf"),(compareTest2 comp2_isProperSubmapOfBy,"comp2_isProperSubmapOfBy")]

propOList = testList "Test/GMap.hs" "propO_" "SimpleTest "
propOs = [(SimpleTest propO_keysAsc,"propO_keysAsc"),(SimpleTest propO_keysDesc,"propO_keysDesc"),(SimpleTest propO_elemsAsc,"propO_elemsAsc"),(SimpleTest propO_elemsDesc,"propO_elemsDesc"),(SimpleTest propO_assocsAsc,"propO_assocsAsc"),(SimpleTest propO_assocsDesc,"propO_assocsDesc"),(SimpleTest propO_lazy_foldKeysAsc,"propO_lazy_foldKeysAsc"),(SimpleTest propO_strict_foldKeysAsc',"propO_strict_foldKeysAsc'"),(SimpleTest propO_lazy_foldKeysDesc,"propO_lazy_foldKeysDesc"),(SimpleTest propO_strict_foldKeysDesc',"propO_strict_foldKeysDesc'"),(SimpleTest propO_lazy_foldElemsAsc,"propO_lazy_foldElemsAsc"),(SimpleTest propO_strict_foldElemsAsc',"propO_strict_foldElemsAsc'"),(SimpleTest propO_lazy_foldElemsDesc,"propO_lazy_foldElemsDesc"),(SimpleTest propO_strict_foldElemsDesc',"propO_strict_foldElemsDesc'"),(SimpleTest propO_lazy_foldAssocsAsc,"propO_lazy_foldAssocsAsc"),(SimpleTest propO_strict_foldAssocsAsc',"propO_strict_foldAssocsAsc'"),(SimpleTest propO_lazy_foldAssocsDesc,"propO_lazy_foldAssocsDesc"),(SimpleTest propO_strict_foldAssocsDesc',"propO_strict_foldAssocsDesc'"),(SimpleTest propO_nubAscWith,"propO_nubAscWith"),(SimpleTest propO_nubDescWith,"propO_nubDescWith"),(SimpleTest propO_sortAscWith,"propO_sortAscWith"),(SimpleTest propO_sortDescWith,"propO_sortDescWith")]

compOList = testList "Test/GMap.hs" "compO_" "compareTest "
compOs = [(compareTest compO_fromAssocsAscWith,"compO_fromAssocsAscWith"),(compareTest compO_fromAssocsDescWith,"compO_fromAssocsDescWith"),(compareTest compO_fromAssocsAscMaybe,"compO_fromAssocsAscMaybe"),(compareTest compO_fromAssocsDescMaybe,"compO_fromAssocsDescMaybe"),(compareTest compO_insertAssocsAscWith,"compO_insertAssocsAscWith"),(compareTest compO_insertAssocsDescWith,"compO_insertAssocsDescWith"),(compareTest compO_insertAssocsAscMaybe,"compO_insertAssocsAscMaybe"),(compareTest compO_insertAssocsDescMaybe,"compO_insertAssocsDescMaybe"),(compareTest compO_foldElemsAsc,"compO_foldElemsAsc"),(compareTest compO_foldElemsDesc,"compO_foldElemsDesc"),(compareTest compO_foldElemsAsc',"compO_foldElemsAsc'"),(compareTest compO_foldElemsDesc',"compO_foldElemsDesc'"),(compareTest compO_foldKeysAsc,"compO_foldKeysAsc"),(compareTest compO_foldKeysDesc,"compO_foldKeysDesc"),(compareTest compO_foldKeysAsc',"compO_foldKeysAsc'"),(compareTest compO_foldKeysDesc',"compO_foldKeysDesc'"),(compareTest compO_foldAssocsAsc,"compO_foldAssocsAsc"),(compareTest compO_foldAssocsDesc,"compO_foldAssocsDesc"),(compareTest compO_foldAssocsAsc',"compO_foldAssocsAsc'"),(compareTest compO_foldAssocsDesc',"compO_foldAssocsDesc'"),(compareTest compO_elemsAsc,"compO_elemsAsc"),(compareTest compO_elemsDesc,"compO_elemsDesc"),(compareTest compO_keysAsc,"compO_keysAsc"),(compareTest compO_keysDesc,"compO_keysDesc"),(compareTest compO_assocsAsc,"compO_assocsAsc"),(compareTest compO_assocsDesc,"compO_assocsDesc")]

unorderedTests = props ++ prop2s ++ comps ++ comp2s -- Cant currently run tests on unordered maps. Easily changed if you complain at me
allTests = props ++ propOs ++ prop2s ++ comps ++ compOs ++ comp2s

-- ### Some ready made test types ###

testSList 		= undefined :: OList Int (Int,Int)
testUnitMap 		= undefined :: UnitMap Int
testEitherMap 		= undefined :: EitherMap (OList Int) (OList Bool) Int
testMaybeMap 		= undefined :: MaybeMap (OList Int) Int
testOrdMap 		= undefined :: OrdMap Int Int
testEnumMap 		= undefined :: EnumMap Bool Int
testIntMap 		= undefined :: IntMap Int
-- testListMap 		= undefined :: ListMap (OList Int) Int
-- testListOrdMap 		= undefined :: ListMap (OrdMap Char) Int
-- testListIntMap 		= undefined :: ListMap IntMap Int
-- testSerialMap 		= undefined :: SerialMap Int Int
-- testSerialMap2 		= undefined :: SerialMap String Int -- !!! Define arbitrary for some more interesting serialisable types.
-- testCacheKeysSerialMap 	= undefined :: CacheKeys (SerialMap String) String Int
testTuple2Map 		= undefined :: Tuple2Map (OList Int) (EnumMap Bool) Int
testTuple3Map 		= undefined :: Tuple3Map (OList Int) (EnumMap Bool) IntMap Int
testTuple4Map 		= undefined :: Tuple4Map (OList Int) (EnumMap Bool) IntMap (OrdMap Char) Int
testTuple5Map 		= undefined :: Tuple5Map (OList Int) (EnumMap Bool) IntMap (OrdMap Char) (OrdMap String) Int
testChoice2Map 		= undefined :: Choice2Map (OList Int) (EnumMap Bool) Int
testChoice3Map 		= undefined :: Choice3Map (OList Int) (EnumMap Bool) IntMap Int
testChoice4Map 		= undefined :: Choice4Map (OList Int) (EnumMap Bool) IntMap (OrdMap Char) Int
testChoice5Map 		= undefined :: Choice5Map (OList Int) (EnumMap Bool) IntMap (OrdMap Char) (OrdMap String) Int
-- testBitMap              = undefined :: SafeBitMap Int
-- testUnrollMap           = undefined :: UnrollMap Int