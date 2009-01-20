{-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction -Wall -fno-warn-missing-signatures #-}

module Data.GMap.TupleMap
(-- * Tuple2Map type
 Tuple2Map
,Tuple3Map
,Tuple4Map
,Tuple5Map
) where

import Prelude hiding (foldr,map,filter,lookup)
import Data.GMap
import Data.GMap.InjectKeys

import Data.Typeable
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import Data.Ord
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import qualified Data.List as L (foldr,foldl')
import Data.Maybe hiding (mapMaybe)

import GHC.Base hiding (map)
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

import qualified Data.List as L
import Control.Monad (mplus)

--------------------------------------------------------------------------------------------
--                     Map Type for tuples and various helper functions                     --
--------------------------------------------------------------------------------------------

data Tuple2Map map1 map2 a = Tuple2Map !(map1 (map2 a))
-- Maintain the invariant that the nested maps are non-empty
emptyInnerMapError funName = error ("Data.GMap.Tuple2Map." ++ funName ++ ": Empty inner map")

-- | Tuple2Map is an instance of Map.
instance (Map map1, Map map2) => Map (Tuple2Map map1 map2) where
	type Key (Tuple2Map map1 map2) = (Key map1, Key map2)

	empty                 	= emptyTuple2Map
	singleton             	= singletonTuple2Map
-- 	pair                  	= pairTuple2Map
	nonEmpty              	= nonEmptyTuple2Map
	status                	= statusTuple2Map
	addSize               	= addSizeTuple2Map
	lookup                	= lookupTuple2Map
	lookupCont            	= lookupContTuple2Map
	alter			= alterTuple2Map
	insertWith            	= insertWithTuple2Map 
	insertWith'           	= insertWithTuple2Map'
	insertMaybe           	= insertMaybeTuple2Map
-- 	fromAssocsWith	        = fromAssocsWithTuple2Map
-- 	fromAssocsMaybe 	= fromAssocsMaybeTuple2Map
	delete                	= deleteTuple2Map 
	adjustWith           	= adjustWithTuple2Map
	adjustWith' 		= adjustWithTuple2Map'
	adjustMaybe		= adjustMaybeTuple2Map
	venn			= vennTuple2Map
	venn'			= vennTuple2Map'
	vennMaybe		= vennMaybeTuple2Map
	disjointUnion		= disjointUnionTuple2Map
	union                 	= unionTuple2Map
	union'                	= unionTuple2Map'
	unionMaybe            	= unionMaybeTuple2Map
	intersection          	= intersectionTuple2Map
	intersection'         	= intersectionTuple2Map'
	intersectionMaybe     	= intersectionMaybeTuple2Map
	difference            	= differenceTuple2Map
	differenceMaybe       	= differenceMaybeTuple2Map
	isSubsetOf            	= isSubsetOfTuple2Map
	isSubmapOf            	= isSubmapOfTuple2Map 
	map                   	= mapTuple2Map
	map'                  	= mapTuple2Map'
	mapMaybe              	= mapMaybeTuple2Map
	mapWithKey            	= mapWithKeyTuple2Map
	mapWithKey'           	= mapWithKeyTuple2Map'
	filter                	= filterTuple2Map
	foldKeys		= foldKeysTuple2Map
	foldElems 		= foldElemsTuple2Map
	foldAssocs		= foldAssocsTuple2Map
	foldKeys'		= foldKeysTuple2Map'
	foldElems' 		= foldElemsTuple2Map'
	foldAssocs'		= foldAssocsTuple2Map'
	foldElemsUInt         	= foldElemsUIntTuple2Map
	valid                 	= validTuple2Map
 
instance (OrderedMap map1, OrderedMap map2) => OrderedMap (Tuple2Map map1 map2) where
	compareKey 	= compareKeyTuple2Map
	fromAssocsAscWith = fromAssocsAscWithTuple2Map
	fromAssocsDescWith = fromAssocsDescWithTuple2Map
	fromAssocsAscMaybe = fromAssocsAscMaybeTuple2Map
	fromAssocsDescMaybe = fromAssocsDescMaybeTuple2Map
 	foldElemsAsc	= foldElemsAscTuple2Map
	foldElemsDesc	= foldElemsDescTuple2Map
	foldKeysAsc	= foldKeysAscTuple2Map
	foldKeysDesc	= foldKeysDescTuple2Map
	foldAssocsAsc	= foldAssocsAscTuple2Map
	foldAssocsDesc	= foldAssocsDescTuple2Map
	foldElemsAsc'	= foldElemsAscTuple2Map'
	foldElemsDesc'	= foldElemsDescTuple2Map'
	foldKeysAsc'	= foldKeysAscTuple2Map'
	foldKeysDesc'	= foldKeysDescTuple2Map'
	foldAssocsAsc'	= foldAssocsAscTuple2Map'
	foldAssocsDesc'	= foldAssocsDescTuple2Map'
	
on f g a b = f $ g a b
	
emptyTuple2Map = Tuple2Map empty
singletonTuple2Map (k1,k2) a = Tuple2Map (singleton k1 (singleton k2 a))

nonEmptyTuple2Map (Tuple2Map mp) = Tuple2Map `fmap` nonEmpty mp

statusTuple2Map (Tuple2Map mp) = 
	case status mp of
		None -> None
		One k1 mp' -> case status mp' of
				None -> emptyInnerMapError "status"
				One k2 a -> One (k1,k2) a
				Many -> Many
		Many -> Many 

addSizeTuple2Map (Tuple2Map mp) i = foldElemsUInt addSize i mp

lookupTuple2Map (k1,k2) (Tuple2Map mp) = lookupCont (lookup k2) k1 mp
lookupContTuple2Map f (k1,k2) (Tuple2Map mp) = lookupCont (lookupCont f k2) k1 mp

alterTuple2Map f (k1,k2) (Tuple2Map mp) = Tuple2Map (alter' alt k1 mp)
 where alt Nothing = singleton k2 `fmap` (f Nothing)
       alt (Just mp') = nonEmpty (alter f k2 mp') 

insertWithTuple2Map  f (k1,k2) a (Tuple2Map mp) = Tuple2Map (insertWith' (insertWith  f k2 a) k1 (singleton k2 a) mp)
insertWithTuple2Map' f (k1,k2) a (Tuple2Map mp) = Tuple2Map (insertWith' (insertWith' f k2 a) k1 (singleton k2 a) mp)
insertMaybeTuple2Map f (k1,k2) a (Tuple2Map mp) = Tuple2Map (insertMaybe' (nonEmpty . insertMaybe f k2 a) k1 (singleton k2 a) mp)

deleteTuple2Map (k1,k2) (Tuple2Map mp) = Tuple2Map (adjustMaybe' (nonEmpty . delete k2) k1 mp)

adjustWithTuple2Map  f (k1,k2) (Tuple2Map mp) = Tuple2Map (adjustWith' (adjustWith  f k2) k1 mp)
adjustWithTuple2Map' f (k1,k2) (Tuple2Map mp) = Tuple2Map (adjustWith' (adjustWith' f k2) k1 mp)
adjustMaybeTuple2Map f (k1,k2) (Tuple2Map mp) = Tuple2Map (adjustMaybe' (nonEmpty . adjustMaybe f k2) k1 mp)

vennTuple2Map f (Tuple2Map mp1) (Tuple2Map mp2) = (Tuple2Map leftDiff, Tuple2Map inter, Tuple2Map rightDiff)
 where	leftDiff  = disjointUnion mpl (mapMaybe (\(l,_,_) -> nonEmpty l) mpi)
 	inter =			      (mapMaybe (\(_,i,_) -> nonEmpty i) mpi)
 	rightDiff = disjointUnion mpr (mapMaybe (\(_,_,r) -> nonEmpty r) mpi)
 	(mpl,mpi,mpr) = venn' (venn f) mp1 mp2

vennTuple2Map' f (Tuple2Map mp1) (Tuple2Map mp2) = (Tuple2Map leftDiff, Tuple2Map inter, Tuple2Map rightDiff)
 where	leftDiff  = disjointUnion mpl (mapMaybe (\(l,_,_) -> nonEmpty l) mpi)
 	inter =			      (mapMaybe (\(_,i,_) -> nonEmpty i) mpi)
 	rightDiff = disjointUnion mpr (mapMaybe (\(_,_,r) -> nonEmpty r) mpi)
 	(mpl,mpi,mpr) = venn' (venn' f) mp1 mp2

vennMaybeTuple2Map f (Tuple2Map mp1) (Tuple2Map mp2) = (Tuple2Map leftDiff, Tuple2Map inter, Tuple2Map rightDiff)
 where	leftDiff  = disjointUnion mpl (mapMaybe (\(l,_,_) -> nonEmpty l) mpi)
 	inter =			      (mapMaybe (\(_,i,_) -> nonEmpty i) mpi)
 	rightDiff = disjointUnion mpr (mapMaybe (\(_,_,r) -> nonEmpty r) mpi)
 	(mpl,mpi,mpr) = venn' (vennMaybe f) mp1 mp2
 	
disjointUnionTuple2Map (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (union' disjointUnion mp1 mp2)
unionTuple2Map  f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (union' (union  f) mp1 mp2)
unionTuple2Map' f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (union' (union' f) mp1 mp2)
unionMaybeTuple2Map f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (unionMaybe' (nonEmpty `on` unionMaybe f) mp1 mp2)

intersectionTuple2Map  f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (intersectionMaybe' (nonEmpty `on` intersection  f) mp1 mp2)
intersectionTuple2Map' f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (intersectionMaybe' (nonEmpty `on` intersection' f) mp1 mp2)
intersectionMaybeTuple2Map f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (intersectionMaybe' (nonEmpty `on` intersectionMaybe f) mp1 mp2)

differenceTuple2Map (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (differenceMaybe' (nonEmpty `on` difference) mp1 mp2) 
differenceMaybeTuple2Map f (Tuple2Map mp1) (Tuple2Map mp2) = Tuple2Map (differenceMaybe' (nonEmpty `on` differenceMaybe f) mp1 mp2) 

isSubsetOfTuple2Map   (Tuple2Map mp1) (Tuple2Map mp2) = isSubmapOf isSubsetOf     mp1 mp2
isSubmapOfTuple2Map f (Tuple2Map mp1) (Tuple2Map mp2) = isSubmapOf (isSubmapOf f) mp1 mp2

mapTuple2Map  f (Tuple2Map mp) = Tuple2Map (map' (map  f) mp)
mapTuple2Map' f (Tuple2Map mp) = Tuple2Map (map' (map' f) mp)
mapMaybeTuple2Map f (Tuple2Map mp) = Tuple2Map (mapMaybe' (nonEmpty . mapMaybe f) mp)
mapWithKeyTuple2Map  f (Tuple2Map mp) = Tuple2Map (mapWithKey' (\k1 mp' -> mapWithKey  (\k2 a -> f (k1,k2) a) mp') mp)
mapWithKeyTuple2Map' f (Tuple2Map mp) = Tuple2Map (mapWithKey' (\k1 mp' -> mapWithKey' (\k2 a -> f (k1,k2) a) mp') mp)

filterTuple2Map f (Tuple2Map mp) = Tuple2Map (mapMaybe' (nonEmpty . filter f) mp)

foldKeysTuple2Map  f b (Tuple2Map mp) = foldAssocs  (\k1 mp' b' -> foldKeys  (\k2 b'' -> f (k1,k2) b'') b' mp') b mp
foldKeysTuple2Map' f b (Tuple2Map mp) = foldAssocs' (\k1 mp' b' -> foldKeys' (\k2 b'' -> f (k1,k2) b'') b' mp') b mp
foldKeysAscTuple2Map  f b (Tuple2Map mp) = foldAssocsAsc  (\k1 mp' b' -> foldKeysAsc  (\k2 b'' -> f (k1,k2) b'') b' mp') b mp
foldKeysAscTuple2Map' f b (Tuple2Map mp) = foldAssocsAsc' (\k1 mp' b' -> foldKeysAsc' (\k2 b'' -> f (k1,k2) b'') b' mp') b mp
foldKeysDescTuple2Map  f b (Tuple2Map mp) = foldAssocsDesc  (\k1 mp' b' -> foldKeysDesc  (\k2 b'' -> f (k1,k2) b'') b' mp') b mp
foldKeysDescTuple2Map' f b (Tuple2Map mp) = foldAssocsDesc' (\k1 mp' b' -> foldKeysDesc' (\k2 b'' -> f (k1,k2) b'') b' mp') b mp

foldElemsTuple2Map  f b (Tuple2Map mp) = foldElems  (\mp' b' -> foldElems  f b' mp') b mp
foldElemsTuple2Map' f b (Tuple2Map mp) = foldElems' (\mp' b' -> foldElems' f b' mp') b mp
foldElemsAscTuple2Map  f b (Tuple2Map mp) = foldElemsAsc  (\mp' b' -> foldElemsAsc  f b' mp') b mp
foldElemsAscTuple2Map' f b (Tuple2Map mp) = foldElemsAsc' (\mp' b' -> foldElemsAsc' f b' mp') b mp
foldElemsDescTuple2Map  f b (Tuple2Map mp) = foldElemsDesc  (\mp' b' -> foldElemsDesc  f b' mp') b mp
foldElemsDescTuple2Map' f b (Tuple2Map mp) = foldElemsDesc' (\mp' b' -> foldElemsDesc' f b' mp') b mp

foldAssocsTuple2Map  f b (Tuple2Map mp) = foldAssocs  (\k1 mp' b' -> foldAssocs  (\k2 a b'' -> f (k1,k2) a b'') b' mp') b mp
foldAssocsTuple2Map' f b (Tuple2Map mp) = foldAssocs' (\k1 mp' b' -> foldAssocs' (\k2 a b'' -> f (k1,k2) a b'') b' mp') b mp
foldAssocsAscTuple2Map  f b (Tuple2Map mp) = foldAssocsAsc  (\k1 mp' b' -> foldAssocsAsc  (\k2 a b'' -> f (k1,k2) a b'') b' mp') b mp
foldAssocsAscTuple2Map' f b (Tuple2Map mp) = foldAssocsAsc' (\k1 mp' b' -> foldAssocsAsc' (\k2 a b'' -> f (k1,k2) a b'') b' mp') b mp
foldAssocsDescTuple2Map  f b (Tuple2Map mp) = foldAssocsDesc  (\k1 mp' b' -> foldAssocsDesc  (\k2 a b'' -> f (k1,k2) a b'') b' mp') b mp
foldAssocsDescTuple2Map' f b (Tuple2Map mp) = foldAssocsDesc' (\k1 mp' b' -> foldAssocsDesc' (\k2 a b'' -> f (k1,k2) a b'') b' mp') b mp

foldElemsUIntTuple2Map f b (Tuple2Map mp) = foldElemsUInt (\mp' b' -> foldElemsUInt f b' mp') b mp

-- Util function for fromAssocs
-- Note that the fold is building difference lists
clump [] = []
clump kas = clumps' [(k',c' [])]
 where  (k', c', clumps') = L.foldl' f (fst $ fst $ head kas,id,id) kas
 	f (currentKey,currentClump,clumps) ((k1,k2),a) =
		if 	k1 == currentKey
		then	(currentKey, currentClump . ((k2,a):), clumps                                   )
		else	(k1,         ((k2,a):),                clumps . ((currentKey,currentClump []):) )

fromAssocsAscWithTuple2Map  f kkas = Tuple2Map (fromAssocsAsc  [(k1,fromAssocsAscWith f kas)  | (k1,kas) <- clump kkas])
fromAssocsDescWithTuple2Map f kkas = Tuple2Map (fromAssocsDesc [(k1,fromAssocsDescWith f kas) | (k1,kas) <- clump kkas])

fromAssocsAscMaybeTuple2Map  f kkas = Tuple2Map (mapMaybe' nonEmpty (fromAssocsAsc  [(k1,fromAssocsAscMaybe f kas)  | (k1,kas) <- clump kkas]))
fromAssocsDescMaybeTuple2Map f kkas = Tuple2Map (mapMaybe' nonEmpty (fromAssocsDesc [(k1,fromAssocsDescMaybe f kas) | (k1,kas) <- clump kkas]))

validTuple2Map (Tuple2Map mp) = 
	case valid mp of
		Nothing -> foldElems (\mp' b -> valid mp' `mplus` b) Nothing mp
		je -> je

compareKeyTuple2Map tmp (k1a,k2a) (k1b,k2b) =
	case compareKey (firstMap tmp) k1a k1b of
		LT -> LT
		EQ -> case compareKey (secondMap tmp) k2a k2b of
			LT -> LT
			EQ -> EQ
			GT -> GT
		GT -> GT
 where 	firstMap :: Tuple2Map map1 map2 a -> map1 a
 	firstMap _ = undefined
 	secondMap :: Tuple2Map map1 map2 a -> map2 a
 	secondMap _ = undefined
 	
--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance Eq (map1 (map2 a)) => Eq (Tuple2Map map1 map2 a) where
 Tuple2Map mapa == Tuple2Map mapb = mapa == mapb

---------
-- Ord --
---------
instance (Map map1, Map map2, Ord (map1 (map2 a))) => Ord (Tuple2Map map1 map2 a) where
 compare (Tuple2Map mapa) (Tuple2Map mapb) = compare mapa mapb

----------
-- Show --
----------
instance (Map map1, Map map2, Show (Key map1), Show (Key map2), Show a) => Show (Tuple2Map map1 map2 a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocs " . shows (assocs mp)

----------
-- Read --
----------
instance (Map map1, Map map2, R.Read (Key map1), R.Read (Key map2), R.Read a) => R.Read (Tuple2Map map1 map2 a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocs" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocs xs)
 readListPrec = R.readListPrecDefault

------------------------
-- Typeable/Typeable1 --
------------------------
instance (Typeable1 map1, Typeable1 map2) => Typeable1 (Tuple2Map map1 map2) where
 typeOf1 m = mkTyConApp (mkTyCon "Data.GMap.TupleMap.Tuple2Map") [typeOf1 map]
  where Tuple2Map map = m -- This is just to get types for map1 & map2 !!
--------------
instance (Typeable1 (Tuple2Map map1 map2), Typeable a) => Typeable (Tuple2Map map1 map2 a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance (Map map1, Map map2) => Functor (Tuple2Map map1 map2) where
-- fmap :: (a -> b) -> Tuple2Map map1 map2 a -> Tuple2Map map1 map2 b
   fmap = mapTuple2Map -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (Map map1, Map map2, M.Monoid a) => M.Monoid (Tuple2Map map1 map2 a) where
-- mempty :: Tuple2Map map1 map2 a
   mempty = emptyTuple2Map
-- mappend :: Tuple2Map map1 map2 a -> Tuple2Map map1 map2 a -> Tuple2Map map1 map2 a
   mappend map0 map1 = unionTuple2Map M.mappend map0 map1
-- mconcat :: [Tuple2Map map1 map2 a] -> Tuple2Map map1 map2 a
   mconcat maps = L.foldr (unionTuple2Map M.mappend) emptyTuple2Map maps

-------------------
-- Data.Foldable --
-------------------
instance (Map map1, Map map2) => F.Foldable (Tuple2Map map1 map2) where
-- fold :: Monoid m => Tuple2Map map1 map2 m -> m
   fold mp = foldElemsTuple2Map M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> Tuple2Map map1 map2 a -> m
   foldMap f mp = foldElemsTuple2Map (\a b -> M.mappend (f a) b) M.mempty mp
-- fold :: (a -> b -> b) -> b -> Tuple2Map map1 map2 a -> b
   foldr f b0 mp = foldElemsTuple2Map f b0 mp
-- foldl :: (a -> b -> a) -> a -> Tuple2Map map1 map2 b -> a
   foldl f b0 mp = foldElemsTuple2Map (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- fold1 :: (a -> a -> a) -> Tuple2Map map1 map2 a -> a
   fold1 = undefined
-- foldl1 :: (a -> a -> a) -> Tuple2Map map1 map2 a -> a
   foldl1 = undefined
-}

-------------------------------------------------------------------------------

-- Larger tuples are mapped recursively

data InjectTuple3 a b c

instance Injection (InjectTuple3 a b c) (a,(b,c)) where
	type K1 (InjectTuple3 a b c) = (a,b,c)
	inject _ (a,b,c) = (a,(b,c))
	outject _ (a,(b,c)) = (a,b,c)
	
type Tuple3Map mapa mapb mapc = 
	InjectKeys 
		(InjectTuple3 (Key mapa) (Key mapb) (Key mapc)) 
		((Key mapa),((Key mapb),(Key mapc))) 
		(Tuple2Map mapa 
			(Tuple2Map mapb mapc))
			
			
			
data InjectTuple4 a b c d

instance Injection (InjectTuple4 a b c d) (a,(b,(c,d))) where
	type K1 (InjectTuple4 a b c d) = (a,b,c,d)
	inject _ (a,b,c,d) = (a,(b,(c,d)))
	outject _ (a,(b,(c,d))) = (a,b,c,d)
	
type Tuple4Map mapa mapb mapc mapd = 
	InjectKeys 
		(InjectTuple4 (Key mapa) (Key mapb) (Key mapc) (Key mapd))
		((Key mapa),((Key mapb),((Key mapc),(Key mapd)))) 
		(Tuple2Map mapa 
			(Tuple2Map mapb 
				(Tuple2Map mapc mapd)))
			
			
			
data InjectTuple5 a b c d e

instance Injection (InjectTuple5 a b c d e) (a,(b,(c,(d,e)))) where
	type K1 (InjectTuple5 a b c d e) = (a,b,c,d,e)
	inject _ (a,b,c,d,e) = (a,(b,(c,(d,e))))
	outject _ (a,(b,(c,(d,e)))) = (a,b,c,d,e)
	
type Tuple5Map mapa mapb mapc mapd mape = 
	InjectKeys 
		(InjectTuple5 (Key mapa) (Key mapb) (Key mapc) (Key mapd) (Key mape))
		((Key mapa),((Key mapb),((Key mapc),((Key mapd),(Key mape))))) 
		(Tuple2Map mapa 
			(Tuple2Map mapb 
				(Tuple2Map mapc 
					(Tuple2Map mapd mape))))