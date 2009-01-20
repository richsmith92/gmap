{-# OPTIONS_GHC -fglasgow-exts -Wall #-}

module Data.GMap (
Map
,Key
,empty
,singleton
,pair
,fromAssocsWith
,fromAssocsMaybe
,status
,nonEmpty
,addSize
,lookup
,lookupCont
,alter
,insertWith
,insertWith'
,insertMaybe
,delete
,adjustWith
,adjustWith'
,adjustMaybe
,venn
,venn'
,vennMaybe
,union
,union'
,unionMaybe
,disjointUnion
,intersection
,intersection'
,intersectionMaybe
,difference
,differenceMaybe
,isSubsetOf
,isSubmapOf
,Data.GMap.map
,map'
,mapMaybe
,mapWithKey
,mapWithKey'
,Data.GMap.filter
,foldElems
,foldKeys
,foldAssocs
,foldElems'
,foldKeys'
,foldAssocs'
,foldElemsUInt
,valid
,disjointUnionError
,Status(None,One,Many)
,vennMaybe'
,alter'
,adjustMaybe'
,insertMaybe'
,unionMaybe'
,intersectionMaybe'
,differenceMaybe'
,mapMaybe'
,isEmpty
,isSingleton
,insert
,insert'
,size
,insertAssocs
,insertAssocsWith
,insertAssocsMaybe
,fromAssocs
,lookupM
,keys
,elems
,assocs
,OrderedMap
,compareKey
,fromAssocsAscWith
,fromAssocsAscMaybe
,fromAssocsDescWith
,fromAssocsDescMaybe
,foldElemsAsc
,foldElemsDesc
,foldKeysAsc
,foldKeysDesc
,foldAssocsAsc
,foldAssocsDesc
,foldElemsAsc'
,foldElemsDesc'
,foldKeysAsc'
,foldKeysDesc'
,foldAssocsAsc'
,foldAssocsDesc'
,fromAssocsAsc
,fromAssocsDesc
,insertAssocsAsc
,insertAssocsDesc
,insertAssocsAscWith
,insertAssocsDescWith
,insertAssocsAscMaybe
,insertAssocsDescMaybe
,elemsAsc
,elemsDesc
,assocsAsc
,assocsDesc
,keysAsc
,keysDesc
,isProperSubsetOf
,isProperSubmapOfBy
-- Partitions are not implemented yet
-- ,partition
-- ,partitionMaybe
-- ,partitionAscList
-- ,partitionDescList
-- ,partitionAscListMaybe
-- ,partitionDescListMaybe
,sortAscWith
,sortDescWith
,nubAscWith
,nubDescWith
)
where

-- import Data.Foldable
-- import Data.Traversable
import GHC.Base
import qualified Data.List as L
import Prelude hiding (map,lookup)

import Control.Monad
import Data.Maybe(maybe)

forceMaybe :: Maybe a -> Maybe a
forceMaybe Nothing = Nothing
forceMaybe (Just a) = a `seq` Just a

on :: (c -> d) -> (a -> b -> c) -> a -> b -> d
on f g a b = f $ g a b

-- | Type of composable maps.
-- For an example of a composed map see Data.GMap.ListMap
class (Eq (Key map)) => Map map where

	type Key map

	-- | The empty map.
	empty :: map a

	-- | Create a map with a single association.
	singleton :: Key map -> a -> map a
	singleton k a = insert k a empty

	-- | Compare two keys and if they are /different/ return a function that will create
	-- a map with two associations (when supplied with the corresponding associated values).
	-- If the keys are the same then this function returns 'Nothing'.
	pair :: Key map -> Key map -> Maybe (a -> a -> map a)
	pair k1 k2 = if k1 == k2 then Nothing else Just (\a1 a2 -> fromAssocs [(k1,a1),(k2,a2)])

	-- | Create a map from an unordered list of associations
	-- Combine repeated keys with the provided function.
	fromAssocsWith :: (a -> a -> a) -> [(Key map,a)] -> map a
	fromAssocsWith f as = L.foldl' (\mp (k,a) -> insertWith (flip f a) k a mp) empty as

	--- | Create a map from an unordered list of associations
	-- Combine repeated keys with the provided function. If the result is Nothing the key is discarded.
	fromAssocsMaybe :: (a -> a -> Maybe a) -> [(Key map,a)] -> map a
	fromAssocsMaybe f as = L.foldl' (\mp (k,a) -> insertMaybe (flip f a) k a mp) empty as

	-- | See the 'Status' type.
	-- This function provides a way to find out if a map is empty, a singleton,
	-- or contains more than one association.
	-- It is useful if empty or singleton maps require special treatment.
	status :: map a -> Status (Key map) a

	-- | Reject empty maps (return Nothing).
	-- Typically used for dealing with nested maps.
	-- eg to delete a key from a nested map:
	-- 'adjustMaybe (nonEmpty $ delete k2) k1'
	nonEmpty :: map a -> Maybe (map a)
	nonEmpty mp = case (status mp) of
		None 	-> Nothing
		_	-> Just mp

	-- | Add number of key\/value pairs in the map to the supplied Int
	addSize :: map a -> Int# -> Int#

	-- | Return the value associated with the supplied key (if any).
	lookup :: Key map -> map a -> Maybe a

	-- | Find the value associated with the supplied key (if any) and return the result
	-- of applying the supplied continuation function to that value. Useful for nested lookup.
	lookupCont :: (a -> Maybe b) -> Key map -> map a -> Maybe b
	lookupCont f k mp = f =<< lookup k mp

	-- | This is a combined insert\/modify\/delete operation. The argument to the supplied function
	-- is ('Just' a) if there is a value (a) associated with the supplied key, otherwise 'Nothing'.
	-- If the return value is ('Just' a'), a' becomes the new value associated with the supplied key.
	-- If the return value is 'Nothing', the association for the supplied key (if any) is deleted.
	alter :: (Maybe a -> Maybe a) -> Key map -> map a -> map a

	-- | Insert a new association in the map if there is currently no value associated with the key.
	-- If there is a value associated with the key then replace it with the result of
	-- applying the supplied function to that value.
	insertWith :: (a -> a) -> Key map -> a -> map a -> map a
	insertWith f k a = alter (Just . maybe a f) k

	-- | Same as 'insertWith', but applies the supplied function strictly if the search succeeds.
	-- Note that the third argument is not strictly evaluated either way (TODO change this)
	insertWith' :: (a -> a) -> Key map -> a -> map a -> map a
	insertWith' f k a = alter' (Just . maybe a f) k

	-- | Similar to 'insert', but the association is deleted if the supplied function returns 'Nothing'.
	-- (The supplied function is always applied strictly.)
	insertMaybe :: (a -> Maybe a) -> Key map -> a -> map a -> map a
	insertMaybe f k a = alter ins k
		where 	ins Nothing = Just a
			ins (Just a') = f a'

	-- | Delete the association for the supplied key (if any).
	delete :: Key map -> map a -> map a
	delete = alter (const Nothing)

	-- | Find the value associated with the supplied key (if any) and apply the supplied function
	-- to that value.
	adjustWith :: (a -> a) -> Key map -> map a -> map a
	adjustWith f = alter (liftM f)

	-- | Same as 'adjust' but applies the supplied function strictly.
	adjustWith' :: (a -> a) -> Key map -> map a -> map a
	adjustWith' f = alter' (fmap f)

	-- | Find the value associated with the supplied key (if any) and apply the supplied function
	-- to that value. Delete the association if the result is 'Nothing'. Replace the old value with
	-- the new value if the result is ('Just' something).
	adjustMaybe :: (a -> Maybe a) -> Key map -> map a -> map a
	adjustMaybe f = alter (f =<<)

	-- | Returns the left difference, intersection and right difference of the supplied maps
	venn :: (a -> b -> c) -> map a -> map b -> (map a, map c, map b)
	venn f = vennMaybe (Just `on` f)

	-- | Same as 'venn', but the new values in the intersection are evaluated strictly
	venn' :: (a -> b -> c) -> map a -> map b -> (map a, map c, map b)
	venn' f = vennMaybe ((forceMaybe . Just) `on` f)

	-- | Same as 'venn', except that values for which the argument function returns nothing
	-- are dropped from the intersection
	vennMaybe :: (a -> b -> Maybe c) -> map a -> map b -> (map a, map c, map b)

	-- | Evaluate the union of two maps. If the maps contain common keys then combine the
	-- values associated with those keys using the supplied function. The value arguments
	-- to this function are supplied in the same order as the map arguments.
	union :: (a -> a -> a) -> map a -> map a -> map a
	union f = unionMaybe (Just `on` f)

	-- | Same as 'unionWith', but the new associated values are evaluated strictly.
	union' :: (a -> a -> a) -> map a -> map a -> map a
	union' f = unionMaybe ((forceMaybe . Just) `on` f)

	-- | Evaluate the union of two maps, but delete combined associations from the result map
	-- if the combining function returns 'Nothing'.
	unionMaybe :: (a -> a -> Maybe a) -> map a -> map a -> map a
	unionMaybe f mpa mpb = disjointUnion leftDiff (disjointUnion inter rightDiff)
		where (leftDiff,inter,rightDiff) = vennMaybe f mpa mpb

	-- | Evaluate the union of two key-disjoint maps. If the arguments are not disjoint the
	-- behaviour is undefined. This is potentially faster than 'union'.
	disjointUnion :: map a -> map a -> map a
	disjointUnion = union' (\ _ _ -> error ("Data.GMap.disjointUnion: Duplicate key found in map."))

	-- | Evaluate the intersection of two maps, combining common associations using the supplied function.
	intersection :: (a -> b -> c) -> map a -> map b -> map c
	intersection f = intersectionMaybe (Just `on` f)

	-- | Same as 'intersection', but the new associated values are evaluated strictly.
	intersection' :: (a -> b -> c) -> map a -> map b -> map c
	intersection' f = intersectionMaybe ((forceMaybe . Just) `on` f)

	-- | Evaluate the intersection of two maps, but delete combined associations from the result map
	-- if the combining function returns 'Nothing'.
	intersectionMaybe :: (a -> b -> Maybe c) -> map a -> map b -> map c
	intersectionMaybe f mpa mpb = inter
		where (_,inter,_) = vennMaybe f mpa mpb

	-- | Evaluate the difference between two maps. For any key occuring in the second map,
	-- the corresponding association (if any) is deleted from the first map.
	-- The associated values in the second map are irrelevant.
	difference :: map a -> map b -> map a
	difference = differenceMaybe (\ _ _ -> Nothing)

	-- | Difference with a combining function. If the combining function returns
	-- @Just a@ then the corresponding association is not deleted from the result map
	-- (it is retained with @a@ as the associated value).
	differenceMaybe :: (a -> b -> Maybe a) -> map a -> map b -> map a
	differenceMaybe f mpa mpb = disjointUnion leftDiff inter
		where (leftDiff,inter,_) = vennMaybe f mpa mpb

	-- | Returns true if the keys in the first map are a subset of the keys in the second map.
	-- (This includes the case where the key sets are identical). Note that this function does
	-- not examine the associated values (which are irrelevant). See 'isSubmapOf' if you
	-- do want associated values examined.
	isSubsetOf :: map a -> map b -> Bool

	-- | Returns true if the keys in the first map are a subset of the keys in the second map
	-- and the corresponding function always returns true when applied to the values associated
	-- with matching keys.
	isSubmapOf :: (a -> b -> Bool) -> map a -> map b -> Bool

	-- | Apply the supplied function to every associated value in the map.
	map :: (a -> b) -> map a -> map b
	map f = mapMaybe (Just . f)

	-- | Same as 'Data.GMap.map', but the function is applied strictly.
	map' :: (a -> b) -> map a -> map b
	map' f = mapMaybe' (Just . f)

	-- | Apply the supplied function to every associated value in the map.
	-- If the result is 'Nothing' then the delete the corresponding association.
	mapMaybe :: (a -> Maybe b) -> map a -> map b

	-- | Apply the supplied function to every association in the map, and use the result
	-- as the new associated value for the corresponding key.
	mapWithKey :: (Key map -> a -> b) -> map a -> map b

	-- | Same as 'mapWithKey', but the function is applied strictly.
	mapWithKey' :: (Key map -> a -> b) -> map a -> map b

	-- | Delete associations for which the supplied predicate returns 'False' when applied to
	-- the associated value.
	filter :: (a -> Bool) -> map a -> map a

	-- | Fold right over the list of elements in an unspecified order.
	foldElems :: (a -> b -> b) -> b -> map a -> b
	foldElems f = foldAssocs (const f)

	-- | Fold right over the list of keys in an unspecified order.
	foldKeys :: (Key map -> b -> b) -> b -> map a -> b
	foldKeys f = foldAssocs (\ k _ -> f k)

	-- | Fold right over the list of associations in an unspecified order.
	foldAssocs :: (Key map -> a -> b -> b) -> b -> map a -> b

	-- | A strict version of 'foldElems' which should be used for
	-- accumulating functions which are strict in their second argument.
	foldElems' :: (a -> b -> b) -> b -> map a -> b
	foldElems' f = foldAssocs' (const f)

	-- | A strict version of 'foldKeys' which should be used for
	-- accumulating functions which are strict in their second argument.
	foldKeys' :: (Key map -> b -> b) -> b -> map a -> b
	foldKeys' f = foldAssocs' (\ k _ -> f k)

	-- | A strict version of 'foldAssocs' which should be used for
	-- accumulating functions which are strict in their third argument.
	foldAssocs' :: (Key map -> a -> b -> b) -> b -> map a -> b

	-- | Fold over elements in un-specified order using /unboxed/ Int accumulator (with GHC).
	-- Defaults to boxed Int for other Haskells. Typically used for counting functions.
	-- Implementations are free to traverse the map in any order.
	-- The folded function is always applied strictly.
	foldElemsUInt :: (a -> Int# -> Int#)-> Int# -> map a  -> Int#

	-- | Test whatever underlying data structure is used to implement an
	-- instance of this class is valid. Used for debugging.
	-- 'Nothing' indicates the data structure is valid.
	valid :: map a -> Maybe String

-- | Raised by disjointUnion if the arguments are not disjoint. Note that instances of Map are *not* required
-- to test that arguments are disjoint.
disjointUnionError = error "Data.GMap.disjointUnion: Arguments not disjoint"

-- | This is the return type for the 'status' method of the 'Map' class
data Status k a = None | One k a | Many deriving Eq

-- | Same as 'vennMaybe' except that the new associated values are strictly evaluated.
vennMaybe' :: Map map => (a -> b -> Maybe c) -> map a -> map b -> (map a, map c, map b)
vennMaybe' f = vennMaybe (forceMaybe `on` f)

-- | Like 'alter' except that the new associated value is strictly evaluated
alter' :: Map map => (Maybe a -> Maybe a) -> Key map -> map a -> map a
alter' f = alter (forceMaybe . f)

-- | Like 'adjustMaybe' except that the new associated value is strictly evaluated
adjustMaybe' :: Map map => (a -> Maybe a) -> Key map -> map a -> map a
adjustMaybe' f = adjustMaybe (forceMaybe . f)

-- | Like 'insertMaybe' except that if the key is already present the new associated
-- value is evaluated strictly. If the key is not present then the supplied value is
-- *not* evaluated strictly. (TODO Change this)
insertMaybe' :: Map map => (a -> Maybe a) -> Key map -> a -> map a -> map a
insertMaybe' f = insertMaybe (forceMaybe . f)

-- | Like 'unionMaybe' except that the new associated values are strictly evaluated
unionMaybe' :: Map map => (a -> a -> Maybe a) -> map a -> map a -> map a
unionMaybe' f = unionMaybe (forceMaybe `on` f)

-- | Like 'intersectionMaybe' except that the new associated values are strictly evaluated
intersectionMaybe' :: Map map => (a -> b -> Maybe c) -> map a -> map b -> map c
intersectionMaybe' f = intersectionMaybe (forceMaybe `on` f)

-- | Like 'differenceMaybe' except that the new associated values are strictly evaluated
differenceMaybe' :: Map map => (a -> b -> Maybe a) -> map a -> map b -> map a
differenceMaybe' f = differenceMaybe (forceMaybe `on` f)

-- | Like 'mapMaybe' except that the new associated values are strictly evaluated
mapMaybe' :: Map map => (a -> Maybe b) -> map a -> map b
mapMaybe' f = mapMaybe (forceMaybe . f)

isEmpty :: Map map => map a -> Bool
isEmpty mp = case (status mp) of
	None 	-> True
	_	-> False

isSingleton :: Map map => map a -> Bool
isSingleton mp = case (status mp) of
	One _ _ -> True
	_	-> False

-- | Write a new association in the map, overwriting any value currently associated with the key.
insert :: Map map => Key map -> a -> map a -> map a
insert k a mp = insertWith (const a) k a mp

-- | Write a new association in the map, overwriting any value currently associated with the key.
-- The new value is evaluated strictly.
insert' :: Map map => Key map -> a -> map a -> map a
insert' k a mp = insertWith' (const a) k a mp

-- | Count the number of associations in a map.
size :: Map map => map a -> Int
size mp = I# (addSize mp 0#)
{-# INLINE size #-}

-- | Insert an unordered list of key\/value pairs into a map.
-- Repeated keys will be overwritten by the last occurence of the key.
insertAssocs :: Map map => [(Key map,a)] -> map a -> map a
insertAssocs = insertAssocsWith (flip const)

insertAssocsWith :: Map map => (a -> a -> a) -> [(Key map,a)] -> map a -> map a
insertAssocsWith f as mp = union f mp (fromAssocsWith f as)

insertAssocsMaybe :: Map map => (a -> a -> Maybe a) -> [(Key map,a)] -> map a -> map a
insertAssocsMaybe f as mp = unionMaybe f mp (fromAssocsMaybe f as)

fromAssocs :: Map map => [(Key map,a)] -> map a
fromAssocs = fromAssocsWith (flip const)

-- | Monadic lookup.
lookupM :: (Map map, Monad m) => Key map -> map a -> m a
lookupM k mp = case lookup k mp of
               Just a  -> return a
               Nothing -> fail "Data.Trie.General.lookupM: Key not found."
{-# SPECIALIZE lookupM :: Map map => Key map -> map a -> Maybe a #-}

keys :: Map map => map a -> [Key map]
keys = foldKeys (:) []

elems :: Map map => map a -> [a]
elems = foldElems (:) []

assocs :: Map map => map a -> [(Key map,a)]
assocs = foldAssocs (\ k a xs -> (k,a):xs) []

-- | Maps which maintain some order on their keys, determined by compareKey.
class Map map => OrderedMap map where

	-- | Every function in this class must respect the ordering given by compareKey.
	-- The first argument is required for its type only and should not be evaluated.
	compareKey :: map a -> Key map -> Key map -> Ordering

	-- | Create a map from an ascending list of key\/value pairs
	-- Combine repeated keys with the provided function.
	fromAssocsAscWith :: (a -> a -> a) -> [(Key map,a)] -> map a
	fromAssocsAscWith f as = L.foldl' (\mp (k,a) -> insertWith (flip f a) k a mp) empty as

	--- | Create a map from an ascending list of key\/value pairs
	-- Combine repeated keys with the provided function. If the result is Nothing the key is discarded.
	fromAssocsAscMaybe :: (a -> a -> Maybe a) -> [(Key map,a)] -> map a
	fromAssocsAscMaybe f as = L.foldl' (\mp (k,a) -> insertMaybe (flip f a) k a mp) empty as

	-- | Create a map from a descending list of key\/value pairs
	-- Combine repeated keys with the provided function.
	fromAssocsDescWith :: (a -> a -> a) -> [(Key map,a)] -> map a
	fromAssocsDescWith f as = L.foldl' (\mp (k,a) -> insertWith (flip f a) k a mp) empty as

	--- | Create a map from a descending list of key\/value pairs
	-- Combine repeated keys with the provided function. If the result is Nothing the key is discarded.
	fromAssocsDescMaybe :: (a -> a -> Maybe a) -> [(Key map,a)] -> map a
	fromAssocsDescMaybe f as = L.foldl' (\mp (k,a) -> insertMaybe (flip f a) k a mp) empty as

	-- | Right associative fold over the list of elements in ascending order of keys.
	-- See 'foldElemsAsc'' for a strict version of this function.
	foldElemsAsc :: (a -> b -> b) -> b -> map a -> b
	foldElemsAsc f = foldAssocsAsc (const f)

	-- | Right associative fold over the list of elements in descending order of keys.
	-- See 'foldElemsDesc'' for a strict version of this function.
	foldElemsDesc :: (a -> b -> b) -> b -> map a -> b
	foldElemsDesc f = foldAssocsDesc (const f)

	-- | Right associative fold over the list of keys in ascending order.
	-- See 'foldKeysAsc'' for a strict version of this function.
	foldKeysAsc :: (Key map -> b -> b) -> b -> map a -> b
	foldKeysAsc f = foldAssocsAsc (\ k _ -> f k)

	-- | Right associative fold over the list of keys in descending order.
	-- See 'foldKeysDesc'' for a strict version of this function.
	foldKeysDesc :: (Key map -> b -> b) -> b -> map a -> b
	foldKeysDesc f = foldAssocsDesc (\ k _ -> f k)

	-- | Right associative fold over the list of associations in ascending order of keys.
	-- See 'foldAssocsAsc'' for a strict version of this function.
	foldAssocsAsc :: (Key map -> a -> b -> b) -> b -> map a -> b

	-- | Right associative fold over the list of associations in descending order of keys.
	-- See 'foldAssocsDesc'' for a strict version of this function.
	foldAssocsDesc :: (Key map -> a -> b -> b) -> b -> map a -> b

	-- | A strict version of 'foldElemsAsc' which should be used for
	-- accumulating functions which are strict in their second argument.
	foldElemsAsc' :: (a -> b -> b) -> b -> map a -> b
	foldElemsAsc' f z as = foldElemsDesc f' id as z -- Note reversed order
  		where f' a c z' = c $! f a z'

	-- | A strict version of 'foldElemsDesc' which should be used for
	-- accumulating functions which are strict in their second argument.
	foldElemsDesc' :: (a -> b -> b) -> b -> map a -> b
	foldElemsDesc' f z as = foldElemsAsc f' id as z -- Note reversed order
  		where f' a c z' = c $! f a z'

	-- | A strict version of 'foldKeysAsc' which should be used for
	-- accumulating functions which are strict in their second argument.
	foldKeysAsc' :: (Key map -> b -> b) -> b -> map a -> b
	foldKeysAsc' f z ks = foldKeysDesc f' id ks z -- Note reversed order
  		where f' k c z' = c $! f k z'

	-- | A strict version of 'foldKeysDesc' which should be used for
	-- accumulating functions which are strict in their second argument.
	foldKeysDesc' :: (Key map -> b -> b) -> b -> map a -> b
	foldKeysDesc' f z ks = foldKeysAsc f' id ks z -- Note reversed order
  		where f' k c z' = c $! f k z'

	-- | A strict version of 'foldAssocsAsc' which should be used for
	-- accumulating functions which are strict in their third argument.
	foldAssocsAsc' :: (Key map -> a -> b -> b) -> b -> map a -> b
	foldAssocsAsc' f z xs = foldAssocsDesc f' id xs z -- Note reversed order
  		where f' k a c z' = c $! f k a z'

	-- | A strict version of 'foldAssocsDesc' which should be used for
	-- accumulating functions which are strict in their third argument.
	foldAssocsDesc' :: (Key map -> a -> b -> b) -> b -> map a -> b
	foldAssocsDesc' f z xs = foldAssocsAsc f' id xs z -- Note reversed order
  		where f' k a c z' = c $! f k a z'

------------------------------------------------------------------------

fromAssocsAsc :: OrderedMap map => [(Key map,a)] -> map a
fromAssocsAsc = fromAssocsAscWith (flip const)

fromAssocsDesc :: OrderedMap map => [(Key map,a)] -> map a
fromAssocsDesc = fromAssocsDescWith (flip const)

-- | Insert an ascending list of associations into a map
-- Duplicate keys are replaced by the rightmost value
insertAssocsAsc :: OrderedMap map => [(Key map,a)] -> map a -> map a
insertAssocsAsc as = insertAssocsAscWith (flip const) as

-- | Insert a descending list of associations into a map
-- Duplicate keys are replaced by the rightmost value
insertAssocsDesc :: OrderedMap map => [(Key map,a)] -> map a -> map a
insertAssocsDesc as = insertAssocsDescWith (flip const) as

-- | Insert an ascending list of associations into a map
-- Duplicate keys are combined with the supplied function
insertAssocsAscWith :: OrderedMap map => (a -> a -> a) -> [(Key map,a)] -> map a -> map a
insertAssocsAscWith f as mp = union f mp (fromAssocsAscWith f as)

-- | Insert a descending list of associations into a map
-- Duplicate keys are combined with the supplied function
insertAssocsDescWith :: OrderedMap map => (a -> a -> a) -> [(Key map,a)] -> map a -> map a
insertAssocsDescWith f as mp = union f mp (fromAssocsDescWith f as)

-- | Same as 'insertAssocsAscWith' except that if Nothing is returned then the key is discarded
insertAssocsAscMaybe :: OrderedMap map => (a -> a -> Maybe a) -> [(Key map,a)] -> map a -> map a
insertAssocsAscMaybe f as mp = unionMaybe f mp (fromAssocsAscMaybe f as)

-- | Same as 'insertAssocsDescWith' except that if Nothing is returned then the key is discarded
insertAssocsDescMaybe :: OrderedMap map => (a -> a -> Maybe a) -> [(Key map,a)] -> map a -> map a
insertAssocsDescMaybe f as mp = unionMaybe f mp (fromAssocsDescMaybe f as)

-- | List the elements in the map in ascending order of keys.
elemsAsc :: OrderedMap map => map a -> [a]
elemsAsc = foldElemsAsc (:) []
{-# INLINE elemsAsc #-}

-- | List the elements in the map in descending order of keys.
elemsDesc :: OrderedMap map => map a -> [a]
elemsDesc = foldElemsDesc (:) []
{-# INLINE elemsDesc #-}

-- | List all associations in the map in ascending order of keys.
assocsAsc :: OrderedMap map => map a -> [(Key map,a)]
assocsAsc = foldAssocsAsc (\k a kas -> (k,a):kas) []
{-# INLINE assocsAsc #-}

-- | List all associations in the map in descending order of keys.
assocsDesc :: OrderedMap map => map a -> [(Key map,a)]
assocsDesc = foldAssocsDesc (\k a kas -> (k,a):kas) []
{-# INLINE assocsDesc #-}

-- | List all keys in the map in ascending order.
keysAsc :: OrderedMap map => map a -> [Key map]
keysAsc = foldKeysAsc (:) []
{-# INLINE keysAsc #-}

-- | List all keys in the map in descending order.
keysDesc :: OrderedMap map =>  map a -> [Key map]
keysDesc = foldKeysDesc (:) []
{-# INLINE keysDesc #-}

-- | Similar to 'isSubsetOf', but also requires that the size of the second map is
-- greater than the first (so does not include the case where the key sets are identical).
isProperSubsetOf :: Map map =>  map a -> map b -> Bool
isProperSubsetOf mpa mpb = (size mpa < size mpb) && (isSubsetOf mpa mpb)
{-# INLINE isProperSubsetOf #-}

-- | Similar to 'isSubmapOf', but also requires that the size of the second map is
-- greater than the first (so does not include the case where the key sets are identical).
isProperSubmapOfBy :: Map map =>  (a -> b -> Bool) -> map a -> map b -> Bool
isProperSubmapOfBy f mpa mpb = (size mpa < size mpb) && (isSubmapOf f mpa mpb)
{-# INLINE isProperSubmapOfBy #-}

-- | Applies the supplied function to every value in a map to create a new key (type @k1@). The
-- result is a map of new keys to a corresponding /non-empty/ map of old keys (type k0) to values.
-- Unimplemented !!!
partition :: (Map map0, Map map1) => (a -> k1) -> map0 a -> map1 (map0 a)
partition p map0 = undefined
{-# INLINE partition #-}

-- | Similar to 'partition', but associations with values yielding 'Nothing' are discarded.
-- Unimplemented !!!
partitionMaybe :: (Map map0, Map map1) => (a -> Maybe (Key map1)) -> map0 a -> map1 (map0 a)
partitionMaybe p map0 = undefined
{-# INLINE partitionMaybe #-}

-- | Applies the supplied function to every value in a map to create a new key (type @Key map1@). The
-- result is a map of new keys to a corresponding /non-empty/ list of old key\/value association pairs.
-- Each list is in ascending order of old keys (type k0).
-- Unimplemented !!!
partitionAscList :: (OrderedMap map0, Map map1) => (a -> Key map1) -> map0 a -> map1 [(Key map0,a)]
partitionAscList p map0 = foldAssocsDesc' ins empty map0 -- We use Desc!! (strict)
 where ins k0 a map1 = insertWith' ((k0,a):) (p a) [(k0,a)] map1           -- Note use of insert'

-- | Applies the supplied function to every value in a map to create a new key (type @Key map1@). The
-- result is a map of new keys to a corresponding /non-empty/ list of old key\/value association pairs.
-- Each list is in descending order of old keys (type k0).
-- Unimplemented !!!
partitionDescList :: (OrderedMap map0, Map map1) => (a -> Key map1) -> map0 a -> map1 [(Key map0,a)]
partitionDescList p map0 = foldAssocsAsc' ins empty map0 -- We use Asc!! (strict)
 where ins k0 a map1 = insertWith' ((k0,a):) (p a) [(k0,a)] map1           -- Note use of insert'

-- | Similar to 'partitionAscList', but associations with values yielding 'Nothing' are discarded.
-- Unimplemented !!!
partitionAscListMaybe :: (OrderedMap map0, Map map1) => (a -> Maybe (Key map1)) -> map0 a -> map1 [(Key map0,a)]
partitionAscListMaybe p map0 = foldAssocsDesc' ins empty map0  -- We use Desc!! (strict)
 where ins k0 a map1 =  case p a of
                        Nothing -> map1
                        Just k1 -> insertWith' ((k0,a):) k1 [(k0,a)] map1       -- Note use of insert'

-- | Similar to 'partitionDescList', but associations with values yielding 'Nothing' are discarded.
-- Unimplemented !!!
partitionDescListMaybe :: (OrderedMap map0, Map map1) => (a -> Maybe (Key map1)) -> map0 a -> map1 [(Key map0,a)]
partitionDescListMaybe p map0 = foldAssocsAsc' ins empty map0 -- We use Asc!! (strict)
 where ins k0 a map1 = case p a of
                       Nothing -> map1
                       Just k1 -> insertWith' ((k0,a):) k1 [(k0,a)] map1        -- Note use of insert'

like :: a -> a -> a
like a _ = a

-- | Use a map of the supplied type to sort a list of keys into ascending order
-- Slower than nubAscWith, but retains duplicate keys
sortAscWith :: OrderedMap map => map Int -> [Key map] -> [Key map]
sortAscWith mp ks = concat [replicate n k | (k,n) <- as]
 where 	as = assocsAsc $ fromAssocsWith (+) (zip ks $ repeat 1) `like` mp

-- | Use a map of the supplied type to sort a list of keys into descending order
-- Slower than nubDescWith, but retains duplicate keys
sortDescWith :: OrderedMap map => map Int -> [Key map] -> [Key map]
sortDescWith mp ks = concat [replicate n k | (k,n) <- as]
 where 	as = assocsDesc $ fromAssocsWith (+) (zip ks $ repeat 1) `like` mp

-- | Use a map of the supplied type to sort a list of keys into ascending order (eliminating duplicates).
nubAscWith :: OrderedMap map => map () -> [Key map] -> [Key map]
nubAscWith mp ks = keysAsc $ fromAssocs (zip ks $ repeat ()) `like` mp

-- | Use a map of the supplied type to sort a list of keys into descending order (eliminating duplicates).
nubDescWith :: OrderedMap map => map () -> [Key map] -> [Key map]
nubDescWith mp ks = keysDesc $ fromAssocs (zip ks $ repeat ()) `like` mp

-----------------------------------------------------------------------------------------------------------------------------------

-- | Instances of OrdMap must satisfy 'compareKey == Ord.compare'
-- class (OrderedMap map, Ord k) => OrdMap map

