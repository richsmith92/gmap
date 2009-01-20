{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-warn-unused-imports -fallow-undecidable-instances -Wall #-}

module Data.GMap.ChoiceMap
(Choice2(C1of2,C2of2)
,Choice2Map
,Choice3(C1of3,C2of3,C3of3)
,Choice3Map
,Choice4(C1of4,C2of4,C3of4,C4of4)
,Choice4Map
,Choice5(C1of5,C2of5,C3of5,C4of5,C5of5)
,Choice5Map
) where

import Prelude hiding (foldr,map,filter,lookup)
import Data.GMap
import Data.GMap.InjectKeys

import qualified Data.Monoid as M (Monoid(..))
import qualified Data.Foldable as F (Foldable(..))
import Data.Typeable
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import qualified Data.List as L

import GHC.Base hiding (map)
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

data Choice2 a b = C1of2 a | C2of2 b deriving (Eq,Ord,Read,Show)

-- | The 'Map' type for keys of form @('Map' mapL, 'Map' mapR) => 'Choice2' (Key mapL) (Key mapR)@.
data Choice2Map mapL mapR a = Choice2Map !(mapL a) !(mapR a)

-- Needs -fallow-undecidable-instances due to coverage condition
instance (Map mapL, Map mapR) => Map (Choice2Map mapL mapR) where
	type Key (Choice2Map mapL mapR) = Choice2 (Key mapL) (Key mapR)
	
	empty                 	= emptyChoice2Map
	singleton             	= singletonChoice2Map
	pair                  	= pairChoice2Map
	nonEmpty              	= nonEmptyChoice2Map
	status                	= statusChoice2Map
	addSize               	= addSizeChoice2Map
	lookup                	= lookupChoice2Map
	--lookupCont            = lookupContChoice2Map
	alter			= alterChoice2Map
	insertWith            	= insertWithChoice2Map 
	insertWith'           	= insertWithChoice2Map'
	insertMaybe           	= insertMaybeChoice2Map
	fromAssocsWith		= fromAssocsWithChoice2Map 
	fromAssocsMaybe 	= fromAssocsMaybeChoice2Map
	delete                	= deleteChoice2Map 
	adjustWith           	= adjustWithChoice2Map
	adjustWith' 		= adjustWithChoice2Map'
	adjustMaybe		= adjustMaybeChoice2Map
	venn			= vennChoice2Map
	venn'			= vennChoice2Map'
	vennMaybe		= vennMaybeChoice2Map
	disjointUnion		= disjointUnionChoice2Map
	union                 	= unionChoice2Map
	union'                	= unionChoice2Map'
	unionMaybe            	= unionMaybeChoice2Map
	intersection          	= intersectionChoice2Map
	intersection'         	= intersectionChoice2Map'
	intersectionMaybe     	= intersectionMaybeChoice2Map
	difference            	= differenceChoice2Map
	differenceMaybe       	= differenceMaybeChoice2Map
	isSubsetOf            	= isSubsetOfChoice2Map
	isSubmapOf              = isSubmapOfChoice2Map
	map                   	= mapChoice2Map
	map'                  	= mapChoice2Map'
	mapMaybe              	= mapMaybeChoice2Map
	mapWithKey            	= mapWithKeyChoice2Map
	mapWithKey'           	= mapWithKeyChoice2Map'
	filter                	= filterChoice2Map
	foldKeys		= foldKeysChoice2Map
	foldElems 		= foldElemsChoice2Map
	foldAssocs		= foldAssocsChoice2Map
	foldKeys'		= foldKeysChoice2Map'
	foldElems' 		= foldElemsChoice2Map'
	foldAssocs'		= foldAssocsChoice2Map'
	foldElemsUInt         	= foldElemsUIntChoice2Map
	valid                 	= validChoice2Map
 
instance (OrderedMap mapL, OrderedMap mapR) => OrderedMap (Choice2Map mapL mapR) where
	compareKey 	= compareKeyChoice2Map
	fromAssocsAscWith = fromAssocsAscWithChoice2Map
	fromAssocsDescWith = fromAssocsDescWithChoice2Map
	fromAssocsAscMaybe = fromAssocsAscMaybeChoice2Map
	fromAssocsDescMaybe = fromAssocsDescMaybeChoice2Map
	foldElemsAsc	= foldElemsAscChoice2Map
	foldElemsDesc	= foldElemsDescChoice2Map
	foldKeysAsc	= foldKeysAscChoice2Map
	foldKeysDesc	= foldKeysDescChoice2Map
	foldAssocsAsc	= foldAssocsAscChoice2Map
	foldAssocsDesc	= foldAssocsDescChoice2Map
	foldElemsAsc'	= foldElemsAscChoice2Map'
	foldElemsDesc'	= foldElemsDescChoice2Map'
	foldKeysAsc'	= foldKeysAscChoice2Map'
	foldKeysDesc'	= foldKeysDescChoice2Map'
	foldAssocsAsc'	= foldAssocsAscChoice2Map'
	foldAssocsDesc'	= foldAssocsDescChoice2Map'
	
-- | See 'Map' class method 'empty'.
emptyChoice2Map :: (Map mapL, Map mapR) => Choice2Map mapL mapR a
emptyChoice2Map = Choice2Map empty empty

-- | See 'Map' class method 'singleton'.
singletonChoice2Map :: (Map mapL, Map mapR) => Choice2 (Key mapL) (Key mapR) -> a -> Choice2Map mapL mapR a
singletonChoice2Map (C1of2 kL) a = Choice2Map (singleton kL a) empty
singletonChoice2Map (C2of2 kR) a = Choice2Map empty (singleton kR a)

-- | See 'Map' class method 'pair'.
pairChoice2Map :: (Map mapL , Map mapR) => Choice2 (Key mapL) (Key mapR) -> Choice2 (Key mapL) (Key mapR) -> Maybe (a -> a -> Choice2Map mapL mapR a)
pairChoice2Map (C1of2 k0) (C1of2 k1) = case pair k0 k1 of
                                     Nothing -> Nothing
                                     Just f  -> Just (\a0 a1 -> Choice2Map (f a0 a1) empty)
pairChoice2Map (C1of2 kL) (C2of2 kR) = Just (\aL aR -> Choice2Map (singleton kL aL) (singleton kR aR))
pairChoice2Map (C2of2 kR) (C1of2 kL) = Just (\aR aL -> Choice2Map (singleton kL aL) (singleton kR aR))
pairChoice2Map (C2of2 k0) (C2of2 k1) = case pair k0 k1 of
                                     Nothing -> Nothing
                                     Just f  -> Just (\a0 a1 -> Choice2Map empty (f a0 a1))

-- | See 'Map' class method 'nonEmpty'.
nonEmptyChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Maybe (Choice2Map mapL mapR a)
nonEmptyChoice2Map egt = if isEmpty egt then Nothing else Just egt

-- | See 'Map' class method 'status'.
statusChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Status (Choice2 (Key mapL) (Key mapR)) a
statusChoice2Map (Choice2Map mapL mapR) = s (status mapL) (status mapR) where
 s None        None        = None
 s None        (One kR aR) = One (C2of2 kR) aR
 s (One kL aL) None        = One (C1of2 kL) aL
 s _           _           = Many

-- | See 'Map' class method 'addSize'.
addSizeChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Int# -> Int#
addSizeChoice2Map (Choice2Map mapL mapR) n = addSize mapL (addSize mapR n)

-- | See 'Map' class method 'lookup'.
lookupChoice2Map :: (Map mapL , Map mapR) => Choice2 (Key mapL) (Key mapR) -> Choice2Map mapL mapR a -> Maybe a
lookupChoice2Map (C1of2 kL) (Choice2Map mapL _   ) = lookup kL mapL
lookupChoice2Map (C2of2 kR) (Choice2Map _    mapR) = lookup kR mapR

-- | See 'Map' class method 'alter'.
alterChoice2Map :: (Map mapL , Map mapR) => (Maybe a -> Maybe a) -> Choice2 (Key mapL) (Key mapR) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
alterChoice2Map f (C1of2 kL) (Choice2Map mapL mapR) = Choice2Map (alter f kL mapL) mapR
alterChoice2Map f (C2of2 kR) (Choice2Map mapL mapR) = Choice2Map mapL (alter f kR mapR)

-- | See 'Map' class method 'insert'.
insertWithChoice2Map :: (Map mapL , Map mapR) => (a -> a) -> Choice2 (Key mapL) (Key mapR) -> a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
insertWithChoice2Map f (C1of2 kL) a (Choice2Map mapL mapR) = Choice2Map (insertWith f kL a mapL) mapR
insertWithChoice2Map f (C2of2 kR) a (Choice2Map mapL mapR) = Choice2Map mapL (insertWith f kR a mapR)

-- | See 'Map' class method 'insert''.
insertWithChoice2Map' :: (Map mapL , Map mapR) => (a -> a) -> Choice2 (Key mapL) (Key mapR) -> a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
insertWithChoice2Map' f (C1of2 kL) a (Choice2Map mapL mapR) = Choice2Map (insertWith' f kL a mapL) mapR
insertWithChoice2Map' f (C2of2 kR) a (Choice2Map mapL mapR) = Choice2Map mapL (insertWith' f kR a mapR)

-- | See 'Map' class method 'insertMaybe'.
insertMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> Maybe a) -> Choice2 (Key mapL) (Key mapR) -> a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
insertMaybeChoice2Map f (C1of2 kL) a (Choice2Map mapL mapR) = Choice2Map (insertMaybe f kL a mapL) mapR
insertMaybeChoice2Map f (C2of2 kR) a (Choice2Map mapL mapR) = Choice2Map mapL (insertMaybe f kR a mapR)

isC1of2 :: Choice2 a b -> Bool
isC1of2 (C1of2 _) = True
isC1of2 (C2of2 _) = False

isC2of2 :: Choice2 a b -> Bool
isC2of2 (C1of2 _) = False 
isC2of2 (C2of2 _) = True

fromAssocsWithChoice2Map :: (Map mapL , Map mapR) => (a -> a -> a) -> [(Choice2 (Key mapL) (Key mapR),a)] -> Choice2Map mapL mapR a
fromAssocsWithChoice2Map f as = Choice2Map (fromAssocsWith f ls) (fromAssocsWith f rs)
	where	ls = L.map (\((C1of2 k), a) -> (k,a)) lefts
		rs = L.map (\((C2of2 k), a) -> (k,a)) rights
		(lefts,rights) = L.partition (isC1of2 . fst) as
		
fromAssocsMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> a -> Maybe a) -> [(Choice2 (Key mapL) (Key mapR),a)] -> Choice2Map mapL mapR a
fromAssocsMaybeChoice2Map f as = Choice2Map (fromAssocsMaybe f ls) (fromAssocsMaybe f rs)
	where	ls = L.map (\((C1of2 k), a) -> (k,a)) lefts
		rs = L.map (\((C2of2 k), a) -> (k,a)) rights
		(lefts,rights) = L.partition (isC1of2 . fst) as
		
fromAssocsAscWithChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (a -> a -> a) -> [(Choice2 (Key mapL) (Key mapR),a)] -> Choice2Map mapL mapR a
fromAssocsAscWithChoice2Map f as = Choice2Map (fromAssocsAscWith f ls) (fromAssocsAscWith f rs)
	where	ls = L.map (\((C1of2 k), a) -> (k,a)) lefts
		rs = L.map (\((C2of2 k), a) -> (k,a)) rights
		(lefts,rights) = L.span (isC1of2 . fst) as
		
fromAssocsAscMaybeChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (a -> a -> Maybe a) -> [(Choice2 (Key mapL) (Key mapR),a)] -> Choice2Map mapL mapR a
fromAssocsAscMaybeChoice2Map f as = Choice2Map (fromAssocsAscMaybe f ls) (fromAssocsAscMaybe f rs)
	where	ls = L.map (\((C1of2 k), a) -> (k,a)) lefts
		rs = L.map (\((C2of2 k), a) -> (k,a)) rights
		(lefts,rights) = L.span (isC1of2 . fst) as
		
fromAssocsDescWithChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (a -> a -> a) -> [(Choice2 (Key mapL) (Key mapR),a)] -> Choice2Map mapL mapR a
fromAssocsDescWithChoice2Map f as = Choice2Map (fromAssocsDescWith f ls) (fromAssocsDescWith f rs)
	where	ls = L.map (\((C1of2 k), a) -> (k,a)) lefts
		rs = L.map (\((C2of2 k), a) -> (k,a)) rights
		(rights,lefts) = L.span (isC2of2 . fst) as
		
fromAssocsDescMaybeChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (a -> a -> Maybe a) -> [(Choice2 (Key mapL) (Key mapR),a)] -> Choice2Map mapL mapR a
fromAssocsDescMaybeChoice2Map f as = Choice2Map (fromAssocsDescMaybe f ls) (fromAssocsDescMaybe f rs)
	where	ls = L.map (\((C1of2 k), a) -> (k,a)) lefts
		rs = L.map (\((C2of2 k), a) -> (k,a)) rights
		(rights,lefts) = L.span (isC2of2 . fst) as

-- | See 'Map' class method 'delete'.
deleteChoice2Map :: (Map mapL , Map mapR) => Choice2 (Key mapL) (Key mapR) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
deleteChoice2Map (C1of2 kL) (Choice2Map mapL mapR) = Choice2Map (delete kL mapL) mapR
deleteChoice2Map (C2of2 kR) (Choice2Map mapL mapR) = Choice2Map mapL (delete kR mapR)

-- | See 'Map' class method 'adjustWith'.
adjustWithChoice2Map :: (Map mapL , Map mapR) => (a -> a) -> Choice2 (Key mapL) (Key mapR) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
adjustWithChoice2Map f (C1of2 kL) (Choice2Map mapL mapR) = Choice2Map (adjustWith f kL mapL) mapR
adjustWithChoice2Map f (C2of2 kR) (Choice2Map mapL mapR) = Choice2Map mapL (adjustWith f kR mapR)

-- | See 'Map' class method 'adjustWith'.
adjustWithChoice2Map' :: (Map mapL , Map mapR) => (a -> a) -> Choice2 (Key mapL) (Key mapR) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
adjustWithChoice2Map' f (C1of2 kL) (Choice2Map mapL mapR) = Choice2Map (adjustWith' f kL mapL) mapR
adjustWithChoice2Map' f (C2of2 kR) (Choice2Map mapL mapR) = Choice2Map mapL (adjustWith' f kR mapR)

-- | See 'Map' class method 'adjustMaybe'.
adjustMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> Maybe a) -> Choice2 (Key mapL) (Key mapR) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
adjustMaybeChoice2Map f (C1of2 kL) (Choice2Map mapL mapR) = Choice2Map (adjustMaybe f kL mapL) mapR
adjustMaybeChoice2Map f (C2of2 kR) (Choice2Map mapL mapR) = Choice2Map mapL (adjustMaybe f kR mapR)

-- | See 'Map' class method 'venn'.
vennChoice2Map :: (Map mapL , Map mapR) => (a -> b -> c) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> (Choice2Map mapL mapR a, Choice2Map mapL mapR c, Choice2Map mapL mapR b)
vennChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 (Choice2Map leftDiffL leftDiffR, Choice2Map interL interR, Choice2Map rightDiffL rightDiffR)
 where (leftDiffL, interL, rightDiffL) = venn f mapL0 mapL1
       (leftDiffR, interR, rightDiffR) = venn f mapR0 mapR1
       
-- | See 'Map' class method 'venn''.
vennChoice2Map' :: (Map mapL , Map mapR) => (a -> b -> c) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> (Choice2Map mapL mapR a, Choice2Map mapL mapR c, Choice2Map mapL mapR b)
vennChoice2Map' f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 (Choice2Map leftDiffL leftDiffR, Choice2Map interL interR, Choice2Map rightDiffL rightDiffR)
 where (leftDiffL, interL, rightDiffL) = venn' f mapL0 mapL1
       (leftDiffR, interR, rightDiffR) = venn' f mapR0 mapR1
       
-- | See 'Map' class method 'vennMaybe'.
vennMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> b -> Maybe c) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> (Choice2Map mapL mapR a, Choice2Map mapL mapR c, Choice2Map mapL mapR b)
vennMaybeChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 (Choice2Map leftDiffL leftDiffR, Choice2Map interL interR, Choice2Map rightDiffL rightDiffR)
 where (leftDiffL, interL, rightDiffL) = vennMaybe f mapL0 mapL1
       (leftDiffR, interR, rightDiffR) = vennMaybe f mapR0 mapR1

-- | See 'Map' class method 'disjointUnion'.
disjointUnionChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
disjointUnionChoice2Map (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (disjointUnion mapL0 mapL1) (disjointUnion mapR0 mapR1)

-- | See 'Map' class method 'union'.
unionChoice2Map :: (Map mapL , Map mapR) => (a -> a -> a) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
unionChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (union f mapL0 mapL1) (union f mapR0 mapR1)

-- | See 'Map' class method 'union''.
unionChoice2Map' :: (Map mapL , Map mapR) => (a -> a -> a) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
unionChoice2Map' f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (union' f mapL0 mapL1) (union' f mapR0 mapR1)

-- | See 'Map' class method 'unionMaybe'.
unionMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> a -> Maybe a) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
unionMaybeChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (unionMaybe f mapL0 mapL1) (unionMaybe f mapR0 mapR1)

-- | See 'Map' class method 'intersection'.
intersectionChoice2Map :: (Map mapL , Map mapR) => (a -> b -> c) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Choice2Map mapL mapR c
intersectionChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (intersection f mapL0 mapL1) (intersection f mapR0 mapR1)

-- | See 'Map' class method 'intersection''.
intersectionChoice2Map' :: (Map mapL , Map mapR) => (a -> b -> c) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Choice2Map mapL mapR c
intersectionChoice2Map' f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (intersection' f mapL0 mapL1) (intersection' f mapR0 mapR1)

-- | See 'Map' class method 'intersectionMaybe'.
intersectionMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> b -> Maybe c) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Choice2Map mapL mapR c
intersectionMaybeChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (intersectionMaybe f mapL0 mapL1) (intersectionMaybe f mapR0 mapR1)

-- | See 'Map' class method 'difference'.
differenceChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Choice2Map mapL mapR a
differenceChoice2Map (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (difference mapL0 mapL1) (difference mapR0 mapR1)

-- | See 'Map' class method 'differenceMaybe'.
differenceMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> b -> Maybe a) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Choice2Map mapL mapR a
differenceMaybeChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 Choice2Map (differenceMaybe f mapL0 mapL1) (differenceMaybe f mapR0 mapR1)

-- | See 'Map' class method 'isSubsetOf'.
isSubsetOfChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Bool
isSubsetOfChoice2Map (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 isSubsetOf mapL0 mapL1 && isSubsetOf mapR0 mapR1

-- | See 'Map' class method 'isSubmapOf'.
isSubmapOfChoice2Map :: (Map mapL , Map mapR) => (a -> b -> Bool) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b -> Bool
isSubmapOfChoice2Map f (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) =
 isSubmapOf f mapL0 mapL1 && isSubmapOf f mapR0 mapR1

-- | See 'Map' class method 'map'.
mapChoice2Map :: (Map mapL , Map mapR) => (a -> b) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b
mapChoice2Map f (Choice2Map mapL mapR) = Choice2Map (map f mapL) (map f mapR)

-- | See 'Map' class method 'map''.
mapChoice2Map' :: (Map mapL , Map mapR) => (a -> b) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b
mapChoice2Map' f (Choice2Map mapL mapR) = Choice2Map (map' f mapL) (map' f mapR)

-- | See 'Map' class method 'mapMaybe'.
mapMaybeChoice2Map :: (Map mapL , Map mapR) => (a -> Maybe b) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b
mapMaybeChoice2Map f (Choice2Map mapL mapR) = Choice2Map (mapMaybe f mapL) (mapMaybe f mapR)

-- | See 'Map' class method 'mapWithKey'.
mapWithKeyChoice2Map :: (Map mapL , Map mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b
mapWithKeyChoice2Map f (Choice2Map mapL mapR) =
 Choice2Map (mapWithKey (\kL a -> f (C1of2 kL) a) mapL) (mapWithKey (\kR a -> f (C2of2 kR) a) mapR)

-- | See 'Map' class method 'mapWithKey''.
mapWithKeyChoice2Map' :: (Map mapL , Map mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b
mapWithKeyChoice2Map' f (Choice2Map mapL mapR) =
 Choice2Map (mapWithKey' (\kL a -> f (C1of2 kL) a) mapL) (mapWithKey' (\kR a -> f (C2of2 kR) a) mapR)

-- | See 'Map' class method 'filter'.
filterChoice2Map :: (Map mapL , Map mapR) => (a -> Bool) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
filterChoice2Map p (Choice2Map mapL mapR) = Choice2Map (filter p mapL) (filter p mapR)

-- | See 'Map' class method 'foldElems'.
foldElemsChoice2Map :: (Map mapL , Map mapR) => (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldElemsChoice2Map f b (Choice2Map mapL mapR) =
 foldElems f (foldElems f b mapR) mapL

-- | See 'Map' class method 'foldKeys'.
foldKeysChoice2Map :: (Map mapL , Map mapR) => (Choice2 (Key mapL) (Key mapR) -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldKeysChoice2Map f b0 (Choice2Map mapL mapR) =
 foldKeys (\kL b -> f (C1of2 kL) b) (foldKeys (\kR b -> f (C2of2 kR) b) b0 mapR) mapL

-- | See 'Map' class method 'foldAssocs'.
foldAssocsChoice2Map :: (Map mapL , Map mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldAssocsChoice2Map f b0 (Choice2Map mapL mapR) =
 foldAssocs (\kL a b -> f (C1of2 kL) a b) (foldAssocs (\kR a b -> f (C2of2 kR) a b) b0 mapR) mapL

-- | See 'Map' class method 'foldElems''.
foldElemsChoice2Map' :: (Map mapL , Map mapR) => (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldElemsChoice2Map' f b (Choice2Map mapL mapR) =
 (\z -> foldElems' f z mapL) $! foldElems' f b mapR
 
-- | See 'Map' class method 'foldKeys''.
foldKeysChoice2Map' :: (Map mapL , Map mapR) => (Choice2 (Key mapL) (Key mapR) -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldKeysChoice2Map' f b0 (Choice2Map mapL mapR) =
 (\z -> foldKeys' (\kL b -> f (C1of2 kL) b) z mapL) $! foldKeys' (\kR b -> f (C2of2 kR) b) b0 mapR

-- | See 'Map' class method 'foldAssocs''.
foldAssocsChoice2Map' :: (Map mapL , Map mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldAssocsChoice2Map' f b0 (Choice2Map mapL mapR) =
 (\z -> foldAssocs' (\kL a b -> f (C1of2 kL) a b) z mapL) $! foldAssocs' (\kR a b -> f (C2of2 kR) a b) b0 mapR
 
 ------------------------

-- | See 'Map' class method 'foldElemsAsc'.
foldElemsAscChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldElemsAscChoice2Map f b (Choice2Map mapL mapR) =
 foldElemsAsc f (foldElemsAsc f b mapR) mapL

-- | See 'Map' class method 'foldElemsDesc'.
foldElemsDescChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldElemsDescChoice2Map f b (Choice2Map mapL mapR) =
 foldElemsDesc f (foldElemsDesc f b mapL) mapR

-- | See 'Map' class method 'foldKeysAsc'.
foldKeysAscChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldKeysAscChoice2Map f b0 (Choice2Map mapL mapR) =
 foldKeysAsc (\kL b -> f (C1of2 kL) b) (foldKeysAsc (\kR b -> f (C2of2 kR) b) b0 mapR) mapL

-- | See 'Map' class method 'foldKeysDesc'.
foldKeysDescChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldKeysDescChoice2Map f b0 (Choice2Map mapL mapR) =
 foldKeysDesc (\kR b -> f (C2of2 kR) b) (foldKeysDesc (\kL b -> f (C1of2 kL) b) b0 mapL) mapR

-- | See 'Map' class method 'foldAssocsAsc'.
foldAssocsAscChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldAssocsAscChoice2Map f b0 (Choice2Map mapL mapR) =
 foldAssocsAsc (\kL a b -> f (C1of2 kL) a b) (foldAssocsAsc (\kR a b -> f (C2of2 kR) a b) b0 mapR) mapL

-- | See 'Map' class method 'foldAssocsDesc'.
foldAssocsDescChoice2Map :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldAssocsDescChoice2Map f b0 (Choice2Map mapL mapR) =
 foldAssocsDesc (\kR a b -> f (C2of2 kR) a b) (foldAssocsDesc (\kL a b -> f (C1of2 kL) a b) b0 mapL) mapR

-- | See 'Map' class method 'foldElemsAsc''.
foldElemsAscChoice2Map' :: (OrderedMap mapL , OrderedMap mapR) => (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldElemsAscChoice2Map' f b (Choice2Map mapL mapR) =
 (\z -> foldElemsAsc' f z mapL) $! foldElemsAsc' f b mapR

-- | See 'Map' class method 'foldElemsDesc''.
foldElemsDescChoice2Map' :: (OrderedMap mapL , OrderedMap mapR) => (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldElemsDescChoice2Map' f b (Choice2Map mapL mapR) =
 (\z -> foldElemsDesc' f z mapR) $! foldElemsDesc' f b mapL

-- | See 'Map' class method 'foldKeysAsc''.
foldKeysAscChoice2Map' :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldKeysAscChoice2Map' f b0 (Choice2Map mapL mapR) =
 (\z -> foldKeysAsc' (\kL b -> f (C1of2 kL) b) z mapL) $! foldKeysAsc' (\kR b -> f (C2of2 kR) b) b0 mapR

-- | See 'Map' class method 'foldKeysDesc''.
foldKeysDescChoice2Map' :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldKeysDescChoice2Map' f b0 (Choice2Map mapL mapR) =
 (\z -> foldKeysDesc' (\kR b -> f (C2of2 kR) b) z mapR) $! foldKeysDesc' (\kL b -> f (C1of2 kL) b) b0 mapL

-- | See 'Map' class method 'foldAssocsAsc''.
foldAssocsAscChoice2Map' :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldAssocsAscChoice2Map' f b0 (Choice2Map mapL mapR) =
 (\z -> foldAssocsAsc' (\kL a b -> f (C1of2 kL) a b) z mapL) $! foldAssocsAsc' (\kR a b -> f (C2of2 kR) a b) b0 mapR

-- | See 'Map' class method 'foldAssocsDesc''.
foldAssocsDescChoice2Map' :: (OrderedMap mapL , OrderedMap mapR) => (Choice2 (Key mapL) (Key mapR) -> a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
foldAssocsDescChoice2Map' f b0 (Choice2Map mapL mapR) =
 (\z -> foldAssocsDesc' (\kR a b -> f (C2of2 kR) a b) z mapR) $! foldAssocsDesc' (\kL a b -> f (C1of2 kL) a b) b0 mapL

-- | See 'Map' class method 'foldElemsUInt'.
foldElemsUIntChoice2Map :: (Map mapL , Map mapR) => (a -> Int# -> Int#) -> Int# -> Choice2Map mapL mapR a -> Int#
foldElemsUIntChoice2Map f n (Choice2Map mapL mapR) = foldElemsUInt f (foldElemsUInt f n mapR) mapL 

-- | See 'Map' class method 'valid'.
validChoice2Map :: (Map mapL , Map mapR) => Choice2Map mapL mapR a -> Maybe String
validChoice2Map (Choice2Map mapL mapR) = case valid mapL of
                                     Nothing -> valid mapR
                                     j       -> j

-- | See 'Map' class method 'compareKeys'
compareKeyChoice2Map :: (OrderedMap mapL, OrderedMap mapR) =>
                       Choice2Map mapL mapR a -> Choice2 (Key mapL) (Key mapR) -> Choice2 (Key mapL) (Key mapR) -> Ordering
compareKeyChoice2Map mp (C1of2 k1) (C1of2 k2) = compareKey (leftMap mp) k1 k2
	where 	leftMap :: Choice2Map mapL mapR a -> mapL a
		leftMap = undefined
compareKeyChoice2Map _ (C1of2 _) (C2of2 _) = LT
compareKeyChoice2Map _ (C2of2 _) (C1of2 _) = GT
compareKeyChoice2Map mp (C2of2 k1) (C2of2 k2) = compareKey (rightMap mp) k1 k2
	where	rightMap :: Choice2Map mapL mapR a -> mapR a
		rightMap = undefined
--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance (Eq (mapL a), Eq (mapR a)) => Eq (Choice2Map mapL mapR a) where
 Choice2Map mapL0 mapR0 == Choice2Map mapL1 mapR1 = (mapL0 == mapL1) && (mapR0 == mapR1)

---------
-- Ord --
---------
instance (Map mapL , Map mapR, Ord (mapL a), Ord (mapR a)) => Ord (Choice2Map mapL mapR a) where
 compare (Choice2Map mapL0 mapR0) (Choice2Map mapL1 mapR1) = c (isEmpty mapL0) (isEmpty mapL1) where
  c True  True  = compare mapR0 mapR1
  c True  False = if isEmpty mapR0 then LT else GT
  c False True  = if isEmpty mapR1 then GT else LT
  c False False = case compare mapL0 mapL1 of
                  LT -> LT
                  EQ -> compare mapR0 mapR1
                  GT -> GT

----------
-- Show --
----------
instance (Map mapL , Map mapR, Show (Key mapL), Show (Key mapR), Show a) => Show (Choice2Map mapL mapR a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocs " . shows (assocs mp)

----------
-- Read --
----------
instance (Map mapL , Map mapR, R.Read (Key mapL), R.Read (Key mapR), R.Read a) => R.Read (Choice2Map mapL mapR a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocs" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocs xs)
 readListPrec = R.readListPrecDefault

------------------------
-- Typeable/Typeable1 --
------------------------
instance (Typeable1 mapL, Typeable1 mapR) => Typeable1 (Choice2Map mapL mapR) where
 typeOf1 m = mkTyConApp (mkTyCon "Data.GMap.ChoiceMap.Choice2Map") [typeOf1 mapL, typeOf1 mapR]
  where Choice2Map mapL mapR = m -- This is just to get types for mapL & mapR !!
--------------
instance (Typeable1 (Choice2Map mapL mapR), Typeable a) => Typeable (Choice2Map mapL mapR a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance (Map mapL , Map mapR) => Functor (Choice2Map mapL mapR) where
-- fmap :: (a -> b) -> Choice2Map mapL mapR a -> Choice2Map mapL mapR b
   fmap = mapChoice2Map -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (Map mapL , Map mapR, M.Monoid a) => M.Monoid (Choice2Map mapL mapR a) where
-- mempty :: Choice2Map mapL mapR a
   mempty = emptyChoice2Map
-- mappend :: Choice2Map mapL mapR a -> Choice2Map mapL mapR a -> Choice2Map mapL mapR a
   mappend map0 map1 = unionChoice2Map M.mappend map0 map1
-- mconcat :: [Choice2Map mapL mapR a] -> Choice2Map mapL mapR a
   mconcat maps = L.foldr (unionChoice2Map M.mappend) emptyChoice2Map maps

-------------------
-- Data.Foldable --
-------------------
instance (Map mapL , Map mapR) => F.Foldable (Choice2Map mapL mapR) where
-- fold :: Monoid m => Choice2Map mapL mapR m -> m
   fold mp = foldElemsChoice2Map M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> Choice2Map mapL mapR a -> m
   foldMap f mp = foldElemsChoice2Map (\a b -> M.mappend (f a) b) M.mempty mp
-- fold :: (a -> b -> b) -> b -> Choice2Map mapL mapR a -> b
   foldr f b0 mp = foldElemsChoice2Map f b0 mp
-- foldl :: (a -> b -> a) -> a -> Choice2Map mapL mapR b -> a
   foldl f b0 mp = foldElemsChoice2Map (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- fold1 :: (a -> a -> a) -> Choice2Map mapL mapR a -> a
   fold1 = undefined
-- foldl1 :: (a -> a -> a) -> Choice2Map mapL mapR a -> a
   foldl1 = undefined
-}

-------------------------------------------------------------------------------

data Choice3 a b c = C1of3 a | C2of3 b | C3of3 c deriving (Eq,Ord,Read,Show)

data InjectChoice3 a b c

instance Injection (InjectChoice3 a b c) (Choice2 a (Choice2 b c)) where
	type K1 (InjectChoice3 a b c) = (Choice3 a b c)

	inject _ choice = case choice of
		C1of3 a -> C1of2 a
		C2of3 b -> C2of2 (C1of2 b)
		C3of3 c -> C2of2 (C2of2 c)
	outject _ choice = case choice of
		C1of2 a 	-> C1of3 a
		C2of2 (C1of2 b) -> C2of3 b
		C2of2 (C2of2 c) -> C3of3 c

type Choice3Map mapa mapb mapc =
	InjectKeys (InjectChoice3 (Key mapa) (Key mapb) (Key mapc))
		(Choice2 (Key mapa) (Choice2 (Key mapb) (Key mapc)))
		(Choice2Map mapa 
			(Choice2Map mapb mapc))
		
data Choice4 a b c d = C1of4 a | C2of4 b | C3of4 c | C4of4 d deriving (Eq,Ord,Read,Show)

data InjectChoice4 a b c d

instance Injection (InjectChoice4 a b c d) (Choice2 (Choice2 a b) (Choice2 c d)) where
	type K1 (InjectChoice4 a b c d) = (Choice4 a b c d)

	inject _ choice = case choice of
		C1of4 a -> C1of2 (C1of2 a)
		C2of4 b -> C1of2 (C2of2 b)
		C3of4 c -> C2of2 (C1of2 c)
		C4of4 d -> C2of2 (C2of2 d)
	outject _ choice = case choice of
		C1of2 (C1of2 a) -> C1of4 a
		C1of2 (C2of2 b) -> C2of4 b
		C2of2 (C1of2 c) -> C3of4 c
  		C2of2 (C2of2 d) -> C4of4 d

type Choice4Map mapa mapb mapc mapd =
	InjectKeys (InjectChoice4 (Key mapa) (Key mapb) (Key mapc) (Key mapd)) 
		(Choice2 (Choice2 (Key mapa) (Key mapb)) (Choice2 (Key mapc) (Key mapd)))
		(Choice2Map  
			(Choice2Map mapa mapb)
			(Choice2Map mapc mapd))
		
		
		
data Choice5 a b c d e = C1of5 a | C2of5 b | C3of5 c | C4of5 d | C5of5 e deriving (Eq,Ord,Read,Show)

data InjectChoice5 a b c d e

instance Injection (InjectChoice5 a b c d e) (Choice2 (Choice2 a b) (Choice2 c (Choice2 d e))) where
	type K1 (InjectChoice5 a b c d e) = (Choice5 a b c d e)

	inject _ choice = case choice of
		C1of5 a -> C1of2 (C1of2 a)
		C2of5 b -> C1of2 (C2of2 b)
		C3of5 c -> C2of2 (C1of2 c)
		C4of5 d -> C2of2 (C2of2 (C1of2 d))
		C5of5 e -> C2of2 (C2of2 (C2of2 e))
	outject _ choice = case choice of
		C1of2 (C1of2 a)	        -> C1of5 a
		C1of2 (C2of2 b)         -> C2of5 b
		C2of2 (C1of2 c)         -> C3of5 c
		C2of2 (C2of2 (C1of2 d)) -> C4of5 d
		C2of2 (C2of2 (C2of2 e)) -> C5of5 e
		
type Choice5Map mapa mapb mapc mapd mape =
	InjectKeys (InjectChoice5 (Key mapa) (Key mapb) (Key mapc) (Key mapd) (Key mape)) 
		(Choice2 (Choice2 (Key mapa) (Key mapb)) (Choice2 (Key mapc) (Choice2 (Key mapd) (Key mape))))
		(Choice2Map  
			(Choice2Map mapa mapb)
			(Choice2Map mapc 
				(Choice2Map mapd mape)))