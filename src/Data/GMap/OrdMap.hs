{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-warn-unused-imports -Wall #-}

module Data.GMap.OrdMap
(-- * OrdMap type
 OrdMap
) where

import Data.GMap
import qualified Data.Tree.AVL  as A
import qualified Data.COrdering as C

import qualified Data.Monoid as M (Monoid(..))
import qualified Data.Foldable as F (Foldable(..))
import Data.Typeable
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import qualified Data.List as L
import qualified Data.Maybe as MB
import Control.Monad

import GHC.Base
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

-- | The default 'Map' type any key type which is an instance of 'Ord'.
-- This is a newtype wrapper around @'Data.Tree.AVL.AVL' (k,a)@.
newtype OrdMap k a = OrdMap (A.AVL (k,a))

instance Ord k => Map (OrdMap k) where
	type Key (OrdMap k) = k

	empty                 	= emptyOrdMap
	singleton             	= singletonOrdMap
	pair                  	= pairOrdMap
	nonEmpty              	= nonEmptyOrdMap
	status                	= statusOrdMap
	addSize               	= addSizeOrdMap
	lookup                	= lookupOrdMap
	lookupCont            	= lookupContOrdMap
	alter			= alterOrdMap
	insertWith            	= insertWithOrdMap
	insertWith'           	= insertWithOrdMap'
	insertMaybe           	= insertMaybeOrdMap
--  	fromAssocsWith		= fromAssocsWithOrdMap
--  	fromAssocsMaybe 	= fromAssocsMaybeOrdMap
	delete                	= deleteOrdMap
	adjustWith           	= adjustWithOrdMap
	adjustWith' 		= adjustWithOrdMap'
	adjustMaybe		= adjustMaybeOrdMap
        venn                    = vennOrdMap
        venn'                   = vennOrdMap'
        vennMaybe               = vennMaybeOrdMap
-- 	merge			= mergeOrdMap
	union                 	= unionOrdMap
	union'                	= unionOrdMap'
	unionMaybe            	= unionMaybeOrdMap
        disjointUnion           = disjointUnionOrdMap
	intersection          	= intersectionOrdMap
	intersection'         	= intersectionOrdMap'
	intersectionMaybe     	= intersectionMaybeOrdMap
	difference            	= differenceOrdMap
	differenceMaybe       	= differenceMaybeOrdMap
	isSubsetOf            	= isSubsetOfOrdMap
	isSubmapOf              = isSubmapOfOrdMap
	map                   	= mapOrdMap
	map'                  	= mapOrdMap'
	mapMaybe              	= mapMaybeOrdMap
	mapWithKey            	= mapWithKeyOrdMap
	mapWithKey'           	= mapWithKeyOrdMap'
	filter                	= filterOrdMap
	foldKeys		= foldKeysAscOrdMap
	foldElems 		= foldElemsAscOrdMap
	foldAssocs		= foldAssocsAscOrdMap
	foldKeys'		= foldKeysAscOrdMap'
	foldElems' 		= foldElemsAscOrdMap'
	foldAssocs'		= foldAssocsAscOrdMap'
	foldElemsUInt         	= foldElemsUIntOrdMap
	valid                 	= validOrdMap

instance Ord k => OrderedMap (OrdMap k) where
	compareKey 		= compareKeyOrdMap
	fromAssocsAscWith 	= fromAssocsAscWithOrdMap
	fromAssocsDescWith 	= fromAssocsDescWithOrdMap
	fromAssocsAscMaybe 	= fromAssocsAscMaybeOrdMap
	fromAssocsDescMaybe 	= fromAssocsDescMaybeOrdMap
 	foldElemsAsc		= foldElemsAscOrdMap
	foldElemsDesc		= foldElemsDescOrdMap
	foldKeysAsc		= foldKeysAscOrdMap
	foldKeysDesc		= foldKeysDescOrdMap
	foldAssocsAsc		= foldAssocsAscOrdMap
	foldAssocsDesc		= foldAssocsDescOrdMap
	foldElemsAsc'		= foldElemsAscOrdMap'
	foldElemsDesc'		= foldElemsDescOrdMap'
	foldKeysAsc'		= foldKeysAscOrdMap'
	foldKeysDesc'		= foldKeysDescOrdMap'
	foldAssocsAsc'		= foldAssocsAscOrdMap'
	foldAssocsDesc'		= foldAssocsDescOrdMap'

-- | See 'Map' class method 'empty'.
emptyOrdMap :: OrdMap k a
emptyOrdMap = OrdMap (A.empty)

-- | See 'Map' class method 'singleton'.
singletonOrdMap :: k -> a -> OrdMap k a
singletonOrdMap k a = OrdMap (A.singleton (k,a))
{-# INLINE singletonOrdMap #-}

-- | See 'Map' class method 'nonEmpty'.
nonEmptyOrdMap :: OrdMap k a -> Maybe (OrdMap k a)
nonEmptyOrdMap m@(OrdMap t) = if A.isEmpty t then Nothing else Just m
{-# INLINE nonEmptyOrdMap #-}

-- | See 'Map' class method 'pair'.
pairOrdMap :: Ord k => k -> k -> Maybe (a -> a -> OrdMap k a)
pairOrdMap x y = case compare x y of
                LT -> Just (\ax ay -> OrdMap (A.pair (x,ax) (y,ay)))
                EQ -> Nothing
                GT -> Just (\ax ay -> OrdMap (A.pair (y,ay) (x,ax)))

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

-- | See 'Map' class method 'fromAssocsAscWith'
fromAssocsAscWithOrdMap :: Ord k => (a -> a -> a) -> [(k,a)] -> OrdMap k a
fromAssocsAscWithOrdMap f kas  = OrdMap $ A.asTreeL [ (k,L.foldl1' f as) | (k,as) <- clump kas]

-- | See 'Map' class method 'fromAssocsDescWith'
fromAssocsDescWithOrdMap :: Ord k => (a -> a -> a) -> [(k,a)] -> OrdMap k a
fromAssocsDescWithOrdMap f kas = OrdMap $ A.asTreeR [ (k,L.foldl1' f as) | (k,as) <- clump kas]

-- | See 'Map' class method 'fromAssocsAscMaybe'
fromAssocsAscMaybeOrdMap  :: Ord k => (a -> a -> Maybe a) -> [(k,a)] -> OrdMap k a
fromAssocsAscMaybeOrdMap f kas  = OrdMap $ A.asTreeL $ MB.catMaybes [ fld k as | (k,as) <- clump kas]
	where fld k as = (\a -> (k,a)) `fmap` foldM f (head as) (tail as) -- NB 'as' guaranteed nonempty by clump

-- | See 'Map' class method 'fromAssocsDescMaybe'
fromAssocsDescMaybeOrdMap :: Ord k => (a -> a -> Maybe a) -> [(k,a)] -> OrdMap k a
fromAssocsDescMaybeOrdMap f kas = OrdMap $ A.asTreeR $ MB.catMaybes [ fld k as | (k,as) <- clump kas]
	where fld k as = (\a -> (k,a)) `fmap` foldM f (head as) (tail as) -- NB 'as' guaranteed nonempty by clump

-- | See 'Map' class method 'status'.
statusOrdMap :: OrdMap k a -> Status k a
statusOrdMap (OrdMap t) = case A.tryGetSingleton t of
                        Just (k,a) -> One k a
                        Nothing    -> if A.isEmpty t then None else Many
{-# INLINE statusOrdMap #-}

-- | See 'Map' class method 'addSize'.
addSizeOrdMap :: OrdMap k a -> Int# -> Int#
addSizeOrdMap (OrdMap t) n = A.addSize# n t
{-# INLINE addSizeOrdMap #-}

-- | See 'Map' class method 'Data.GMap.lookup'.
lookupOrdMap :: Ord k => k -> OrdMap k a -> Maybe a
lookupOrdMap k (OrdMap t) = A.tryRead t cmp
 where cmp (k',a) = case compare k k' of
                    LT -> C.Lt
                    EQ -> C.Eq a
                    GT -> C.Gt

-- | See 'Map' class method 'lookupCont'.
lookupContOrdMap :: Ord k => (a -> Maybe b) -> k -> OrdMap k a -> Maybe b
lookupContOrdMap f k (OrdMap t) = A.tryReadMaybe t cmp
 where cmp (k',a) = case compare k k' of
                    LT -> C.Lt
                    EQ -> let mb = f a in mb `seq` C.Eq mb
                    GT -> C.Gt

-- | See 'Map' class method 'alter'.
alterOrdMap :: Ord k => (Maybe a -> Maybe a) -> k -> OrdMap k a -> OrdMap k a
alterOrdMap f k (OrdMap t) = case A.tryReadBAVL bavl of
                           Nothing     -> OrdMap (doIt k  Nothing ) -- bavl is empty
                           Just (k',a) -> OrdMap (doIt k' (Just a)) -- bavl is full
 where bavl = A.openBAVL cmp t
       cmp (k',_)  = compare k k'
       doIt k' mba = case f mba of
                     Nothing -> A.deleteBAVL bavl       -- This is a nop for empty bavl
                     Just a' -> A.pushBAVL (k',a') bavl -- This is a write for full bavl

-- | See 'Map' class method 'insertWith'.
insertWithOrdMap :: Ord k => (a -> a) -> k -> a -> OrdMap k a -> OrdMap k a
insertWithOrdMap f k a (OrdMap t) = OrdMap (A.push cmp (k,a) t)
 where cmp (k',a') = case compare k k' of
                     LT -> C.Lt
                     EQ -> C.Eq (k',f a')
                     GT -> C.Gt

-- | See 'Map' class method 'insertWith'.
insertWithOrdMap' :: Ord k => (a -> a) -> k -> a -> OrdMap k a -> OrdMap k a
insertWithOrdMap' f k a (OrdMap t) = OrdMap (A.push' cmp (a `seq` (k,a)) t) -- Note use of genPush'
 where cmp (k',a') = case compare k k' of
                     LT -> C.Lt
                     EQ -> let b' = f a' in b' `seq` C.Eq (k',f a')
                     GT -> C.Gt

-- | See 'Map' class method 'insertMaybe'.
insertMaybeOrdMap :: Ord k => (a -> Maybe a) -> k -> a -> OrdMap k a -> OrdMap k a
insertMaybeOrdMap f k a (OrdMap t) = case A.tryReadBAVL bavl of
                                   Nothing -> OrdMap (A.pushBAVL (k,a) bavl)
                                   Just (k',a') -> case f a' of
                                                   Nothing  -> OrdMap (A.deleteBAVL bavl)
                                                   Just a'' -> OrdMap (A.pushBAVL (k',a'') bavl)
 where bavl = A.openBAVL cmp t
       cmp (k',_) = compare k k'

-- | See 'Map' class method 'delete'.
deleteOrdMap :: Ord k => k -> OrdMap k a -> OrdMap k a
deleteOrdMap k (OrdMap t) = OrdMap (A.delete cmp t)
 where cmp (k',_) = compare k k'
{-# INLINE deleteOrdMap #-}

-- | See 'Map' class method 'adjust'.
adjustWithOrdMap :: Ord k => (a -> a) -> k -> OrdMap k a -> OrdMap k a
adjustWithOrdMap f k (OrdMap t) = OrdMap (A.deleteMaybe cmp t)
 where cmp (k',a) = case compare k k' of
                    LT -> C.Lt
                    EQ -> C.Eq (Just (k',f a))
                    GT -> C.Gt

-- | See 'Map' class method 'adjust''.
adjustWithOrdMap' :: Ord k => (a -> a) -> k -> OrdMap k a -> OrdMap k a
adjustWithOrdMap' f k (OrdMap t) = OrdMap (A.deleteMaybe cmp t)
 where cmp (k',a) = case compare k k' of
                    LT -> C.Lt
                    EQ -> let a' = f a in a' `seq` C.Eq (Just (k',a'))
                    GT -> C.Gt

-- | See 'Map' class method 'adjustMaybe'.
adjustMaybeOrdMap :: Ord k => (a -> Maybe a) -> k -> OrdMap k a -> OrdMap k a
adjustMaybeOrdMap f k (OrdMap t) = OrdMap (A.deleteMaybe cmp t)
 where cmp (k',a) = case compare k k' of
                    LT -> C.Lt
                    EQ -> case f a of
                          Nothing -> C.Eq Nothing
                          Just a' -> C.Eq (Just (k',a'))
                    GT -> C.Gt

-- | See 'Map' class method 'venn'.
vennOrdMap :: Ord k => (a -> b -> c) -> OrdMap k a -> OrdMap k b -> (OrdMap k a, OrdMap k c, OrdMap k b)
vennOrdMap f (OrdMap t) (OrdMap t') = case A.venn cmp t t' of (tab,ti,tba) -> (OrdMap tab,OrdMap ti,OrdMap tba)
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> C.Eq (k, f a b)
                          GT -> C.Gt

-- | See 'Map' class method 'venn''.
vennOrdMap' :: Ord k => (a -> b -> c) -> OrdMap k a -> OrdMap k b -> (OrdMap k a, OrdMap k c, OrdMap k b)
vennOrdMap' f (OrdMap t) (OrdMap t') = case A.venn cmp t t' of (tab,ti,tba) -> (OrdMap tab,OrdMap ti,OrdMap tba)
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> let c =  f a b in c `seq` C.Eq (k,c)
                          GT -> C.Gt

-- | See 'Map' class method 'vennMaybe'.
vennMaybeOrdMap :: Ord k => (a -> b -> Maybe c) -> OrdMap k a -> OrdMap k b -> (OrdMap k a, OrdMap k c, OrdMap k b)
vennMaybeOrdMap f (OrdMap t) (OrdMap t') = case A.vennMaybe cmp t t' of (tab,ti,tba) -> (OrdMap tab,OrdMap ti,OrdMap tba)
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> case f a b of
                                Nothing -> C.Eq Nothing
                                Just c  -> C.Eq (Just (k,c))
                          GT -> C.Gt

-- | See 'Map' class method 'union'.
unionOrdMap :: Ord k => (a -> a -> a) -> OrdMap k a -> OrdMap k a -> OrdMap k a
unionOrdMap f (OrdMap t) (OrdMap t') = OrdMap (A.union cmp t t')
 where cmp (k,a) (k',a') = case compare k k' of
                           LT -> C.Lt
                           EQ -> C.Eq (k, f a a')
                           GT -> C.Gt

-- | See 'Map' class method 'union''.
unionOrdMap' :: Ord k => (a -> a -> a) -> OrdMap k a -> OrdMap k a -> OrdMap k a
unionOrdMap' f (OrdMap t) (OrdMap t') = OrdMap (A.union cmp t t')
 where cmp (k,a) (k',a') = case compare k k' of
                           LT -> C.Lt
                           EQ -> let a'' = f a a' in a'' `seq` C.Eq (k, a'')
                           GT -> C.Gt

-- | See 'Map' class method 'unionMaybe'.
unionMaybeOrdMap :: Ord k => (a -> a -> Maybe a) -> OrdMap k a -> OrdMap k a -> OrdMap k a
unionMaybeOrdMap f (OrdMap t) (OrdMap t') = OrdMap (A.unionMaybe cmp t t')
 where cmp (k,a) (k',a') = case compare k k' of
                           LT -> C.Lt
                           EQ -> case f a a' of
                                 Nothing  -> C.Eq Nothing
                                 Just a'' -> C.Eq (Just (k,a''))
                           GT -> C.Gt

-- | See 'Map' class method 'disjointUnion'.
disjointUnionOrdMap :: Ord k => OrdMap k a -> OrdMap k a -> OrdMap k a
disjointUnionOrdMap (OrdMap t) (OrdMap t') = OrdMap (A.disjointUnion cmp t t')
 where cmp (k,_) (k',_) = compare k k'

-- | See 'Map' class method 'intersection'.
intersectionOrdMap :: Ord k => (a -> b -> c) -> OrdMap k a -> OrdMap k b -> OrdMap k c
intersectionOrdMap f (OrdMap t) (OrdMap t') = OrdMap (A.intersection cmp t t')
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> C.Eq (k, f a b)
                          GT -> C.Gt

-- | See 'Map' class method 'intersection''.
intersectionOrdMap' :: Ord k => (a -> b -> c) -> OrdMap k a -> OrdMap k b -> OrdMap k c
intersectionOrdMap' f (OrdMap t) (OrdMap t') = OrdMap (A.intersection cmp t t')
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> let c = f a b in c `seq` C.Eq (k, c)
                          GT -> C.Gt

-- | See 'Map' class method 'intersectionMaybe'.
intersectionMaybeOrdMap :: Ord k => (a -> b -> Maybe c) -> OrdMap k a -> OrdMap k b -> OrdMap k c
intersectionMaybeOrdMap f (OrdMap ta) (OrdMap tb) = OrdMap (A.intersectionMaybe cmp ta tb)
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> case f a b of
                                Nothing -> C.Eq Nothing
                                Just c  -> C.Eq (Just (k,c))
                          GT -> C.Gt

-- | See 'Map' class method 'difference'.
differenceOrdMap :: Ord k => OrdMap k a -> OrdMap k b -> OrdMap k a
differenceOrdMap (OrdMap t1) (OrdMap t2) = OrdMap (A.difference cmp t1 t2)
 where cmp (k,_) (k',_) = compare k k'

-- | See 'Map' class method 'differenceMaybe'.
differenceMaybeOrdMap :: Ord k => (a -> b -> Maybe a) -> OrdMap k a -> OrdMap k b -> OrdMap k a
differenceMaybeOrdMap f (OrdMap ta) (OrdMap tb) = OrdMap (A.differenceMaybe cmp ta tb)
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> case f a b of
                                Nothing -> C.Eq Nothing
                                Just a' -> C.Eq (Just (k,a'))
                          GT -> C.Gt

-- | See 'Map' class method 'isSubsetOf'.
isSubsetOfOrdMap :: Ord k => OrdMap k a -> OrdMap k b -> Bool
isSubsetOfOrdMap (OrdMap ta) (OrdMap tb) = A.isSubsetOf cmp ta tb
 where cmp (k,_) (k',_) = compare k k'

-- | See 'Map' class method 'isSubmapOf'.
isSubmapOfOrdMap :: Ord k => (a -> b -> Bool) -> OrdMap k a -> OrdMap k b -> Bool
isSubmapOfOrdMap p (OrdMap ta) (OrdMap tb) = A.isSubsetOfBy cmp ta tb
 where cmp (k,a) (k',b) = case compare k k' of
                          LT -> C.Lt
                          EQ -> C.Eq $! p a b
                          GT -> C.Gt

-- | See 'Map' class method 'Data.GMap.map'.
mapOrdMap :: (a -> b) -> OrdMap k a -> OrdMap k b
-- Note use of strict AVL map! (This does not force evaluation of f a).
mapOrdMap f (OrdMap t) = OrdMap (A.map' (\(k,a) -> (k,f a)) t)
{-# INLINE mapOrdMap #-}

-- | See 'Map' class method 'map''.
mapOrdMap' :: (a -> b) -> OrdMap k a -> OrdMap k b
mapOrdMap' f (OrdMap t) = OrdMap (A.map' (\(k,a) -> let b = f a in b `seq` (k,b)) t)
{-# INLINE mapOrdMap' #-}

-- | See 'Map' class method 'mapMaybe'.
mapMaybeOrdMap :: (a -> Maybe b) -> OrdMap k a -> OrdMap k b
mapMaybeOrdMap f (OrdMap t) = OrdMap (A.mapMaybe f' t)
 where f' (k,a) = case f a of
                  Nothing -> Nothing
                  Just b  -> Just (k,b)

-- | See 'Map' class method 'mapWithKey'.
mapWithKeyOrdMap :: (k -> a -> b) -> OrdMap k a -> OrdMap k b
-- Note use of strict AVL map! (This does not force evaluation of f k a).
mapWithKeyOrdMap f (OrdMap t) = OrdMap (A.map' (\(k,a) -> (k, f k a)) t)
{-# INLINE mapWithKeyOrdMap #-}

-- | See 'Map' class method 'mapWithKey''.
mapWithKeyOrdMap' :: (k -> a -> b) -> OrdMap k a -> OrdMap k b
mapWithKeyOrdMap' f (OrdMap t) = OrdMap (A.map' (\(k,a) -> let b = f k a in b `seq` (k, b)) t)
{-# INLINE mapWithKeyOrdMap' #-}

-- | See 'Map' class method 'Data.GMap.filter'.
filterOrdMap :: (a -> Bool) -> OrdMap k a -> OrdMap k a
filterOrdMap f (OrdMap t) = OrdMap (A.filter (\(_,a) -> f a) t)
{-# INLINE filterOrdMap #-}

-- | See 'Map' class method 'foldElemsAsc'.
foldElemsAscOrdMap :: (a -> b -> b) -> b  -> OrdMap k a-> b
foldElemsAscOrdMap f b0 (OrdMap t) = A.foldr (\(_,a) b -> f a b) b0 t -- Lazy foldr
{-# INLINE foldElemsAscOrdMap #-}

-- | See 'Map' class method 'foldElemsDesc'.
foldElemsDescOrdMap :: (a -> b -> b) -> b -> OrdMap k a -> b
foldElemsDescOrdMap f b0 (OrdMap t) = A.foldl (\b (_,a) -> f a b) b0 t -- Lazy foldl
{-# INLINE foldElemsDescOrdMap #-}

-- | See 'Map' class method 'foldKeysAsc'.
foldKeysAscOrdMap :: (k -> b -> b) -> b -> OrdMap k a -> b
foldKeysAscOrdMap f b0 (OrdMap t) = A.foldr (\(k,_) b -> f k b) b0 t -- Lazy foldr
{-# INLINE foldKeysAscOrdMap #-}

-- | See 'Map' class method 'foldKeysDesc'.
foldKeysDescOrdMap :: (k -> b -> b) -> b -> OrdMap k a -> b
foldKeysDescOrdMap f b0 (OrdMap t) = A.foldl (\b (k,_) -> f k b) b0 t -- Lazy foldl
{-# INLINE foldKeysDescOrdMap #-}

-- | See 'Map' class method 'foldAssocsAsc'.
foldAssocsAscOrdMap :: (k -> a -> b -> b) -> b -> OrdMap k a -> b
foldAssocsAscOrdMap f b0 (OrdMap t) = A.foldr (\(k,a) b -> f k a b) b0 t -- Lazy foldr
{-# INLINE foldAssocsAscOrdMap #-}

-- | See 'Map' class method 'foldAssocsDesc'.
foldAssocsDescOrdMap :: (k -> a -> b -> b) -> b -> OrdMap k a -> b
foldAssocsDescOrdMap f b0 (OrdMap t) = A.foldl (\b (k,a) -> f k a b) b0 t -- Lazy foldl
{-# INLINE foldAssocsDescOrdMap #-}

-- | See 'Map' class method 'foldElemsAsc''.
foldElemsAscOrdMap' :: (a -> b -> b) -> b -> OrdMap k a -> b
foldElemsAscOrdMap' f b0 (OrdMap t) = A.foldr' (\(_,a) b -> f a b) b0 t -- Strict foldr
{-# INLINE foldElemsAscOrdMap' #-}

-- | See 'Map' class method 'foldElemsDesc''.
foldElemsDescOrdMap' :: (a -> b -> b) -> b -> OrdMap k a -> b
foldElemsDescOrdMap' f b0 (OrdMap t) = A.foldl' (\b (_,a) -> f a b) b0 t -- Strict foldl
{-# INLINE foldElemsDescOrdMap' #-}

-- | See 'Map' class method 'foldKeysAsc''.
foldKeysAscOrdMap' :: (k -> b -> b) -> b -> OrdMap k a -> b
foldKeysAscOrdMap' f b0 (OrdMap t) = A.foldr' (\(k,_) b -> f k b) b0 t -- Strict foldr
{-# INLINE foldKeysAscOrdMap' #-}

-- | See 'Map' class method 'foldKeysDesc''.
foldKeysDescOrdMap' :: (k -> b -> b) -> b -> OrdMap k a -> b
foldKeysDescOrdMap' f b0 (OrdMap t) = A.foldl' (\b (k,_) -> f k b) b0 t -- Strict foldl
{-# INLINE foldKeysDescOrdMap' #-}

-- | See 'Map' class method 'foldAssocsAsc''.
foldAssocsAscOrdMap' :: (k -> a -> b -> b) -> b -> OrdMap k a -> b
foldAssocsAscOrdMap' f b0 (OrdMap t) = A.foldr' (\(k,a) b -> f k a b) b0 t -- Strict foldr
{-# INLINE foldAssocsAscOrdMap' #-}

-- | See 'Map' class method 'foldAssocsDesc''.
foldAssocsDescOrdMap' :: (k -> a -> b -> b) -> b -> OrdMap k a -> b
foldAssocsDescOrdMap' f b0 (OrdMap t) = A.foldl' (\b (k,a) -> f k a b) b0 t -- Strict foldl
{-# INLINE foldAssocsDescOrdMap' #-}

-- | See 'Map' class method 'foldElemsUInt'.
foldElemsUIntOrdMap :: (a -> Int# -> Int#) -> Int# -> OrdMap k a -> Int#
foldElemsUIntOrdMap f n (OrdMap t) = A.foldrInt# (\(_,a) u -> f a u) n t
{-# INLINE foldElemsUIntOrdMap #-}

-- | See 'Map' class method 'valid'.
validOrdMap :: Ord k => OrdMap k a -> Maybe String
validOrdMap (OrdMap t) =
 if      A.isSorted (\(k0,_) (k1,_) -> compare k0 k1) t
 then if A.isBalanced t
      then Nothing
      else Just "OrdMap: Tree is not balanced."
 else      Just "OrdMap: Tree is not sorted."

-- | See 'Map' class method 'compareKey'
compareKeyOrdMap :: Ord k => OrdMap k a -> k -> k -> Ordering
compareKeyOrdMap _ = compare

--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance (Eq k, Eq a) => Eq (OrdMap k a) where
 OrdMap t0 == OrdMap t1 = t0 == t1

---------
-- Ord --
---------
instance (Ord k, Ord a) => Ord (OrdMap k a) where
 compare (OrdMap t0) (OrdMap t1) = compare t0 t1

----------
-- Show --
----------
instance (Ord k, Show k, Show a) => Show (OrdMap k a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocsAsc " . shows (assocsAsc mp)

----------
-- Read --
----------
instance (Ord k, R.Read k, R.Read a) => R.Read (OrdMap k a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocsAsc" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocsAsc xs)
 readListPrec = R.readListPrecDefault

------------------------
-- Typeable/Typeable1 --
------------------------
instance (Ord k, Typeable k) => Typeable1 (OrdMap k) where
 typeOf1 mp =  mkTyConApp (mkTyCon "Data.GMap.OrdMap.OrdMap") [typeOf k]
  where [(k,_)]  = assocsAsc mp -- This is just to get type for k !!
--------------
instance (Typeable1 (OrdMap k), Typeable a) => Typeable (OrdMap k a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance Functor (OrdMap k) where
-- fmap :: (a -> b) -> OrdMap k a -> OrdMap k b
   fmap = mapOrdMap -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (Ord k, M.Monoid a) => M.Monoid (OrdMap k a) where
-- mempty :: OrdMap k a
   mempty = emptyOrdMap
-- mappend :: OrdMap k a -> OrdMap k a -> OrdMap k a
   mappend map0 map1 = unionOrdMap M.mappend map0 map1
-- mconcat :: [OrdMap k a] -> OrdMap k a
   mconcat maps = L.foldr (unionOrdMap M.mappend) emptyOrdMap maps

-------------------
-- Data.Foldable --
-------------------
instance F.Foldable (OrdMap k) where
-- fold :: Monoid m => OrdMap k m -> m
   fold mp = foldElemsAscOrdMap M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> OrdMap k a -> m
   foldMap f mp = foldElemsAscOrdMap (\a b -> M.mappend (f a) b) M.mempty mp
-- foldr :: (a -> b -> b) -> b -> OrdMap k a -> b
   foldr f b0 mp = foldElemsAscOrdMap f b0 mp
-- foldl :: (a -> b -> a) -> a -> OrdMap k b -> a
   foldl f b0 mp = foldElemsDescOrdMap (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- foldr1 :: (a -> a -> a) -> OrdMap k a -> a
   foldr1 = undefined
-- foldl1 :: (a -> a -> a) -> OrdMap k a -> a
   foldl1 = undefined
-}
