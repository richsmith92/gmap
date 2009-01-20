{-# OPTIONS_GHC -fglasgow-exts -Wall -fno-warn-orphans -fno-warn-unused-imports -fno-warn-missing-signatures #-}

module Data.GMap.UnitMap
(-- * UnitMap type
 UnitMap
) where

import Data.GMap

import qualified Data.Monoid as M (Monoid(..))
import qualified Data.Foldable as F (Foldable(..))
import Data.Typeable
-- -fno-warn-unused-imports used because ghc currently gives spurious warning with this import
-- See Tickets 1074 and 1148
import qualified Data.List as L (foldr)

import GHC.Base hiding (map)
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

import Data.Maybe

-- | The default 'Map' type unit (empty tuple) keys.
newtype UnitMap a = UnitMap (Maybe a)

instance Map UnitMap where
	type Key UnitMap = ()

	empty                 	= emptyUnitMap
	singleton             	= singletonUnitMap
	pair                  	= pairUnitMap
	nonEmpty              	= nonEmptyUnitMap
	status                	= statusUnitMap
	addSize               	= addSizeUnitMap
	lookup                	= lookupUnitMap
	alter			= alterUnitMap
	vennMaybe		= vennMaybeUnitMap
	unionMaybe		= unionMaybeUnitMap
	isSubsetOf            	= isSubsetOfUnitMap
	isSubmapOf            = isSubmapOfUnitMap
	mapMaybe              	= mapMaybeUnitMap
	mapWithKey            	= mapWithKeyUnitMap
	mapWithKey'           	= mapWithKeyUnitMap'
	filter                	= filterUnitMap
	foldKeys		= foldKeysUnitMap
	foldElems 		= foldElemsUnitMap
	foldAssocs		= foldAssocsUnitMap
	foldKeys'		= foldKeysUnitMap
	foldElems' 		= foldElemsUnitMap
	foldAssocs'		= foldAssocsUnitMap
	foldElemsUInt         	= foldElemsUIntUnitMap
	valid                 	= validUnitMap

instance OrderedMap UnitMap where
	compareKey 	= compareKeyUnitMap
	-- fromAssocsAscWith
	-- fromAssocsDescWith
	-- fromAssocsAscMaybe
	-- fromAssocsDescMaybe
	foldElemsAsc	= foldElemsUnitMap
	foldElemsDesc	= foldElemsUnitMap
	foldKeysAsc	= foldKeysUnitMap
	foldKeysDesc	= foldKeysUnitMap
	foldAssocsAsc	= foldAssocsUnitMap
	foldAssocsDesc	= foldAssocsUnitMap
	foldElemsAsc'	= foldElemsUnitMap
	foldElemsDesc'	= foldElemsUnitMap
	foldKeysAsc'	= foldKeysUnitMap
	foldKeysDesc'	= foldKeysUnitMap
	foldAssocsAsc'	= foldAssocsUnitMap
	foldAssocsDesc'	= foldAssocsUnitMap

-- | See 'Map' class method 'empty'.
emptyUnitMap :: UnitMap a
emptyUnitMap = UnitMap Nothing
{-# INLINE emptyUnitMap #-}

-- | See 'Map' class method 'singleton'.
singletonUnitMap :: () -> a -> UnitMap a
singletonUnitMap _ a = UnitMap (Just a)
{-# INLINE singletonUnitMap #-}

-- | See 'Map' class method 'pair'.
pairUnitMap :: () -> () -> Maybe (a -> a -> UnitMap a)
pairUnitMap _ _ = Nothing -- Args are always equal!!
{-# INLINE pairUnitMap #-}

-- | See 'Map' class method 'nonEmpty'.
nonEmptyUnitMap :: UnitMap a -> Maybe (UnitMap a)
nonEmptyUnitMap (UnitMap Nothing) = Nothing
nonEmptyUnitMap ugt              = Just ugt

-- | See 'Map' class method 'status'.
statusUnitMap :: UnitMap a -> Status () a
statusUnitMap (UnitMap (Just a)) = One () a
statusUnitMap _                 = None

-- | See 'Map' class method 'addSize'.
addSizeUnitMap :: UnitMap a -> Int# -> Int#
addSizeUnitMap (UnitMap Nothing) n = n
addSizeUnitMap _                n = (n +# 1#)

-- | See 'Map' class method 'Data.GMap.lookup'.
lookupUnitMap :: () -> UnitMap a -> Maybe a
lookupUnitMap _ (UnitMap mba) = mba
{-# INLINE lookupUnitMap #-}

alterUnitMap :: (Maybe a -> Maybe a) -> () -> UnitMap a -> UnitMap a
alterUnitMap f _ (UnitMap mba) = UnitMap (f mba)

-- | See 'Map' class method 'vennMaybe'
vennMaybeUnitMap :: (a -> b -> Maybe c) -> UnitMap a -> UnitMap b -> (UnitMap a, UnitMap c, UnitMap b)
vennMaybeUnitMap _ (UnitMap Nothing)  (UnitMap Nothing)  = (UnitMap Nothing, UnitMap Nothing, UnitMap Nothing)
vennMaybeUnitMap _ (UnitMap ja     )  (UnitMap Nothing)  = (UnitMap ja     , UnitMap Nothing, UnitMap Nothing)
vennMaybeUnitMap _ (UnitMap Nothing)  (UnitMap jb     )  = (UnitMap Nothing, UnitMap Nothing, UnitMap jb     )
vennMaybeUnitMap f (UnitMap (Just a)) (UnitMap (Just b)) = (UnitMap Nothing, UnitMap (f a b), UnitMap Nothing)

-- | See 'Map' class method 'unionMaybe'.
unionMaybeUnitMap :: (a -> a -> Maybe a) -> UnitMap a -> UnitMap a -> UnitMap a
unionMaybeUnitMap _ (UnitMap Nothing)  (UnitMap Nothing)  = UnitMap Nothing
unionMaybeUnitMap _ (UnitMap ja     )  (UnitMap Nothing)  = UnitMap ja
unionMaybeUnitMap _ (UnitMap Nothing)  (UnitMap jb     )  = UnitMap jb
unionMaybeUnitMap f (UnitMap (Just a)) (UnitMap (Just b)) = UnitMap (f a b)

-- | See 'Map' class method 'isSubsetOf'.
isSubsetOfUnitMap :: UnitMap a -> UnitMap b -> Bool
isSubsetOfUnitMap (UnitMap Nothing ) _                  = True
isSubsetOfUnitMap (UnitMap (Just _)) (UnitMap (Just _))  = True
isSubsetOfUnitMap _                 _                  = False

-- | See 'Map' class method 'isSubmapOf'.
isSubmapOfUnitMap :: (a -> b -> Bool) -> UnitMap a -> UnitMap b -> Bool
isSubmapOfUnitMap _ (UnitMap Nothing ) _                  = True
isSubmapOfUnitMap f (UnitMap (Just a)) (UnitMap (Just b))  = f a b
isSubmapOfUnitMap _ _                 _                  = False

-- | See 'Map' class method 'Data.GMap.mapMaybe'.
mapMaybeUnitMap :: (a -> Maybe b) -> UnitMap a -> UnitMap b
mapMaybeUnitMap f (UnitMap (Just a)) = UnitMap (f a)
mapMaybeUnitMap _ _                 = emptyUnitMap

-- | See 'Map' class method 'mapWithKey'.
mapWithKeyUnitMap :: (() -> a -> b) -> UnitMap a -> UnitMap b
mapWithKeyUnitMap f (UnitMap (Just a)) = UnitMap (Just (f () a))
mapWithKeyUnitMap _ _                 = emptyUnitMap

-- | See 'Map' class method 'mapWithKey''.
mapWithKeyUnitMap' :: (() -> a -> b) -> UnitMap a -> UnitMap b
mapWithKeyUnitMap' f (UnitMap (Just a)) = let b = f () a in b `seq` UnitMap (Just b)
mapWithKeyUnitMap' _ _                 = emptyUnitMap

-- | See 'Map' class method 'Data.GMap.filter'.
filterUnitMap :: (a -> Bool) -> UnitMap a -> UnitMap a
filterUnitMap p u@(UnitMap (Just a)) = if p a then u else emptyUnitMap
filterUnitMap _   _                 = emptyUnitMap

-- | See 'Map' class method 'foldElems'
foldKeysUnitMap :: (() -> b -> b) -> b -> UnitMap a -> b
foldKeysUnitMap f b (UnitMap mba) = case mba of
	Just _  -> f () b
	Nothing -> b

-- | See 'Map' class method 'foldElems'
foldElemsUnitMap :: (a -> b -> b) -> b -> UnitMap a -> b
foldElemsUnitMap f b (UnitMap mba) = case mba of
	Just a  -> f a b
	Nothing -> b

-- | See 'Map' class method 'foldAssocs'
foldAssocsUnitMap :: (() -> a -> b -> b) -> b -> UnitMap a -> b
foldAssocsUnitMap f b (UnitMap mba) = case mba of
	Just a  -> f () a b
	Nothing -> b

-- | See 'Map' class method 'foldElemsInt#'.
foldElemsUIntUnitMap :: (a -> Int# -> Int#) -> Int# -> UnitMap a -> Int#
foldElemsUIntUnitMap f n (UnitMap mba) = case mba of
	Just a  -> f a n
	Nothing -> n

-- | See 'Map' class method 'valid'.
validUnitMap :: UnitMap a -> Maybe String
validUnitMap _ = Nothing -- Always valid!
{-# INLINE validUnitMap #-}

-- | See 'Map' class method 'compareKey'
compareKeyUnitMap :: UnitMap a -> () -> () -> Ordering
compareKeyUnitMap _ _ _ = EQ

--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------

--------
-- Eq --
--------
instance Eq a => Eq (UnitMap a) where
 UnitMap mba0 == UnitMap mba1 = mba0 == mba1

---------
-- Ord --
---------
instance Ord a => Ord (UnitMap a) where
 compare (UnitMap Nothing  ) (UnitMap Nothing  ) = EQ
 compare (UnitMap Nothing  ) (UnitMap (Just _ )) = LT
 compare (UnitMap (Just _ )) (UnitMap Nothing  ) = GT
 compare (UnitMap (Just a0)) (UnitMap (Just a1)) = compare a0 a1

----------
-- Show --
----------
instance Show a => Show (UnitMap a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocs " . shows (assocs mp)

----------
-- Read --
----------
instance R.Read a => R.Read (UnitMap a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocs" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocs xs)
 readListPrec = R.readListPrecDefault

------------------------
-- Typeable/Typeable1 --
------------------------
instance Typeable1 UnitMap where
 typeOf1 _ = mkTyConApp (mkTyCon "Data.GMap.UnitMap.UnitMap") []
--------------
instance Typeable a => Typeable (UnitMap a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance Functor (UnitMap) where
-- fmap :: (a -> b) -> UnitMap a -> UnitMap b
   fmap = Data.GMap.map -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (M.Monoid a) => M.Monoid (UnitMap a) where
-- mempty :: UnitMap a
   mempty = emptyUnitMap
-- mappend :: UnitMap a -> UnitMap a -> UnitMap a
   mappend map0 map1 = union M.mappend map0 map1
-- mconcat :: [UnitMap a] -> UnitMap a
   mconcat maps = L.foldr (union M.mappend) emptyUnitMap maps

-------------------
-- Data.Foldable --
-------------------
instance F.Foldable (UnitMap) where
-- fold :: Monoid m => UnitMap m -> m
   fold mp = foldElemsUnitMap M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> UnitMap a -> m
   foldMap f mp = foldElemsUnitMap (\a b -> M.mappend (f a) b) M.mempty mp
-- foldr :: (a -> b -> b) -> b -> UnitMap a -> b
   foldr f b0 mp = foldElemsUnitMap f b0 mp
-- foldl :: (a -> b -> a) -> a -> UnitMap b -> a
   foldl f b0 mp = foldElemsUnitMap (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- foldr1 :: (a -> a -> a) -> UnitMap a -> a
   foldr1 = undefined
-- foldl1 :: (a -> a -> a) -> UnitMap a -> a
   foldl1 = undefined
-}
