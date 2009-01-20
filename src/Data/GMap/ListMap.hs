{-# OPTIONS_GHC -fglasgow-exts  -Wall #-} -- -fallow-undecidable-instances

module Data.GMap.ListMap
(-- * ListMap type
 ListMap
) where

import Prelude hiding (foldr,map,filter,lookup)
import Data.GMap

import Data.Typeable
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import Data.Maybe hiding (mapMaybe)

import GHC.Base hiding (map)
import qualified Text.Read as R (Read(..),Lexeme(..),parens,prec,lexP,readListPrecDefault)

import qualified Data.List as L

--------------------------------------------------------------------------------------------
--                     Map Type for lists and various helper functions                     --
--------------------------------------------------------------------------------------------

-- | The 'Map' type for keys of form @'Map' map k => [k]@.
data ListMap map a
 = Empt                                  -- Empty special, never appears in non-empty ListMap!
 | BraF ![Key map] a !(map (ListMap map a))   -- Full branch, tail map may be empty or singleton
 | BraE ![Key map]   !(map (ListMap map a))   -- Empty branch, no empty or singletons allowed.

-- Invariants are:
-- * Tail maps must not contain 'Empt' ListMap elements.
-- * The tail map of a 'BraE' node must contain at least 2 entries.
--   (Empty and singleton tail maps are degenerate cases which are normalised appropriately.)
-- Smart constructor for BraE. Ensures tail is not empty or singleton map.
braE :: Map map => [Key map] -> map (ListMap map a) -> ListMap map a
braE ks mp = case status mp of
             None                   -> Empt
             One _  Empt            -> error "braE: Empty ListMap in tail map."
             One k (BraF ks' a mp') -> BraF (ks ++ k:ks') a mp'
             One k (BraE ks'   mp') -> BraE (ks ++ k:ks')   mp'
             Many                   -> BraE ks mp

-- | ListMap is an instance of Map.
instance Map map => Map (ListMap map) where
	type Key (ListMap map) = [Key map]

	empty                 	= emptyListMap
	singleton             	= singletonListMap
	pair                  	= pairListMap
	nonEmpty              	= nonEmptyListMap
	status                	= statusListMap
	addSize               	= addSizeListMap
	lookup                	= lookupListMap
	lookupCont            	= lookupContListMap
	alter			= alterListMap
	insertWith            	= insertWithListMap 
	insertWith'           	= insertWithListMap'
	insertMaybe           	= insertMaybeListMap
-- 	fromAssocsWith	= fromAssocsWithListMap
-- 	fromAssocsMaybe 	= fromAssocsMaybeListMap
	delete                	= deleteListMap 
	adjustWith           	= adjustWithListMap
	adjustWith' 		= adjustWithListMap'
	adjustMaybe		= adjustMaybeListMap
	venn			= vennListMap
	venn'			= vennListMap'
	vennMaybe		= vennMaybeListMap
-- 	disjointUnion		= disjointUnionListMap
	union                 	= unionListMap
	union'                	= unionListMap'
	unionMaybe            	= unionMaybeListMap
	intersection          	= intersectionListMap
	intersection'         	= intersectionListMap'
	intersectionMaybe     	= intersectionMaybeListMap
	difference            	= differenceListMap
	differenceMaybe       	= differenceMaybeListMap
	isSubsetOf            	= isSubsetOfListMap
	isSubmapOf            	= isSubmapOfListMap 
	map                   	= mapListMap
	map'                  	= mapListMap'
	mapMaybe              	= mapMaybeListMap
	mapWithKey            	= mapWithKeyListMap
	mapWithKey'           	= mapWithKeyListMap'
	filter                	= filterListMap
	foldKeys		= foldKeysListMap
	foldElems 		= foldElemsListMap
	foldAssocs		= foldAssocsListMap
	foldKeys'		= foldKeysListMap'
	foldElems' 		= foldElemsListMap'
	foldAssocs'		= foldAssocsListMap'
	foldElemsUInt         	= foldElemsUIntListMap
	valid                 	= validListMap
 
instance OrderedMap map => OrderedMap (ListMap map) where
	compareKey 	= compareKeyListMap
	fromAssocsAscWith = fromAssocsAscWithListMap
	fromAssocsDescWith = fromAssocsDescWithListMap
	fromAssocsAscMaybe = fromAssocsAscMaybeListMap
	fromAssocsDescMaybe = fromAssocsDescMaybeListMap
 	foldElemsAsc	= foldElemsAscListMap
	foldElemsDesc	= foldElemsDescListMap
	foldKeysAsc	= foldKeysAscListMap
	foldKeysDesc	= foldKeysDescListMap
	foldAssocsAsc	= foldAssocsAscListMap
	foldAssocsDesc	= foldAssocsDescListMap
	foldElemsAsc'	= foldElemsAscListMap'
	foldElemsDesc'	= foldElemsDescListMap'
	foldKeysAsc'	= foldKeysAscListMap'
	foldKeysDesc'	= foldKeysDescListMap'
	foldAssocsAsc'	= foldAssocsAscListMap'
	foldAssocsDesc'	= foldAssocsDescListMap'

-- Strict ++
infixr 5 +!+
(+!+) :: [a] -> [a] -> [a]
[]     +!+ ys = ys
(x:xs) +!+ ys = let xs' = xs +!+ ys in xs' `seq` x:xs'
{- (not used currently)
xs +!+ [] = xs
xs +!+ ys = f xs where f []      = ys
                       f (x:xs') = let xs'' = f xs' in xs'' `seq` x:xs''
-}

-- Local Utility for reverse join: revTo xs ys = (reverse xs) ++ ys
revTo :: [a] -> [a] -> [a]
revTo []     ys = ys
revTo (x:xs) ys = revTo xs (x:ys)

-- Take the first N elements of a list.
-- Gives an error if list is not long enough to do this!
takeN :: Int# -> [Key map] -> [Key map]
takeN 0# _      = []
takeN _    []     = error "Data.GMap.ListMap.takeN: List is too short."
takeN n    (k:ks) = let ks_ = takeN (n -# 1#) ks in ks_ `seq` k:ks_

-- Return type of the match function
-- Do we need the Int# in Sfx and Sfy constructors ??
data Match map a =
   Mat                    -- Input lists match and have same length (I.E. they are identical)
 | Frk Int# (ListMap map a -> ListMap map a -> map (ListMap map a)) (Key (ListMap map)) (Key (ListMap map))       -- n f xs ys
 | Sfx Int# (Key map) (Key (ListMap map))         -- Input lists match but xs has remaining non-empty suffix -- n x xs
 | Sfy Int# (Key map) (Key (ListMap map))         -- Input lists match but ys has remaining non-empty suffix -- n y ys

-- Try to match two lists of keys
match :: Map map => (Key (ListMap map)) -> (Key (ListMap map)) -> Match map a
match xs0 ys0 = m 0# xs0 ys0
 where m _ []     []     = Mat
       m n []     (y:ys) = Sfy n y ys
       m n (x:xs) []     = Sfx n x xs
       m n (x:xs) (y:ys) = case pair x y of
                           Just f  -> Frk n (\mpa mpb -> mpa `seq` mpb `seq` f mpa mpb) xs ys
                           Nothing -> m ((n) +# 1#) xs ys   -- x == y

-- Common error message associated with (supposedly) sorted associations lists.
-- Can be caused by improper sorting (including duplicate keys)
badAssocs :: String
badAssocs = "Data.GMap.ListMap: Bad sorted association List."
--------------------------------------------------------------------------------------------

-- | See 'Map' class method 'empty'.
emptyListMap :: ListMap map a
emptyListMap = Empt
{-# INLINE emptyListMap #-}

-- | See 'Map' class method 'singleton'.
singletonListMap :: Map map => (Key (ListMap map)) -> a -> ListMap map a
singletonListMap ks a = BraF ks a empty
{-# INLINE singletonListMap #-}

-- | See 'Map' class method 'pair'.
pairListMap :: Map map => (Key (ListMap map)) -> (Key (ListMap map)) -> Maybe (a -> a -> ListMap map a)
pairListMap xs0 ys0 = pr 0# xs0 ys0 where
 pr _ []     []     = Nothing
 pr _ []     (y:ys) = Just (\ax ay -> BraF xs0 ax (singleton y (BraF ys ay empty)))
 pr _ (x:xs) []     = Just (\ax ay -> BraF ys0 ay (singleton x (BraF xs ax empty)))
 pr n (x:xs) (y:ys) = case pair x y of
                      Just f  -> Just (\ax ay -> BraE (takeN n xs0) (f (BraF xs ax empty) (BraF ys ay empty)))
                      Nothing -> pr ((n) +# 1#) xs ys

-- | See 'Map' class method 'nonEmpty'.
nonEmptyListMap :: ListMap map a -> Maybe (ListMap map a)
nonEmptyListMap Empt = Nothing
nonEmptyListMap lmp  = Just lmp
{-# INLINE nonEmptyListMap #-}

-- | See 'Map' class method 'status'.
statusListMap :: Map map => ListMap map a -> Status (Key (ListMap map)) a
statusListMap  Empt          = None
statusListMap (BraF ks a mp) = if (isEmpty mp) then (One ks a) else Many
statusListMap (BraE _    _ ) = Many
{-# INLINE statusListMap #-}

-- | See 'Map' class method 'addSize'.
addSizeListMap :: Map map => ListMap map a -> Int# -> Int#
addSizeListMap  Empt         n = n
addSizeListMap (BraF _ _ mp) n = foldElemsUInt addSizeListMap ((n) +# 1#) mp
addSizeListMap (BraE _   mp) n = foldElemsUInt addSizeListMap n mp

-- | See 'Map' class method 'lookup'.
lookupListMap :: Map map => (Key (ListMap map)) -> ListMap map a -> Maybe a
lookupListMap ks0 lmp0 = lmb ks0 lmp0 where
 lmb _ Empt = Nothing
------------------------------
 lmb ks (BraF ks' a mp) = pre ks ks' where
  pre []     []     = Just a
  pre []     (_:_ ) = Nothing
  pre (x:xs) []     = case lookup x mp of
                      Just lmp -> lmb xs lmp
                      Nothing  -> Nothing
  pre (x:xs) (y:ys) = if x == y then pre xs ys else Nothing
------------------------------
 lmb ks (BraE ks' mp) = pre ks ks' where
  pre []     _      = Nothing
  pre (x:xs) []     = case lookup x mp of
                      Just lmp -> lmb xs lmp
                      Nothing  -> Nothing
  pre (x:xs) (y:ys) = if x == y then pre xs ys else Nothing
------------------------------

-- | See 'Map' class method 'lookupCont'.
lookupContListMap :: Map map => (a -> Maybe b) -> (Key (ListMap map)) -> ListMap map a -> Maybe b
-- Convention below is xs is the search key list and ys is the key list fragment from the Trie (ListMap)
lookupContListMap j ks0 lmp0 = lmb ks0 lmp0 where
 lmb _ Empt = Nothing
------------------------------
 lmb ks (BraF ks' a mp) = pre ks ks' where
  pre []     []     = j a
  pre []     (_:_ ) = Nothing
  pre (x:xs) []     = lookupCont (lmb xs) x mp
  pre (x:xs) (y:ys) = if x == y then pre xs ys else Nothing
------------------------------
 lmb ks (BraE ks' mp) = pre ks ks' where
  pre []     _      = Nothing
  pre (x:xs) []     = lookupCont (lmb xs) x mp
  pre (x:xs) (y:ys) = if x == y then pre xs ys else Nothing
------------------------------

-- | See 'Map' class method 'delete'.
deleteListMap :: Map map => (Key (ListMap map)) -> ListMap map a -> ListMap map a
deleteListMap = adjustMaybeListMap (const Nothing)
{-# INLINE deleteListMap #-}

-- | See 'Map' class method 'adjustWith'.
adjustWithListMap :: Map map => (a -> a) -> (Key (ListMap map)) -> ListMap map a -> ListMap map a
-- N.B. One day we will have a more efficient implementation of this
adjustWithListMap f ks0 lmp0 = dmb ks0 lmp0 where
 dmb _ Empt = Empt
------------------------------
 dmb ks bf@(BraF ks' a mp) = pre ks ks' where
  pre []     []     = BraF  ks' (f a) mp
  pre []     (_:_ ) = bf
  pre (x:xs) []     = BraF ks' a (adjustWith (\lmp -> dmb xs lmp) x mp)
  pre (x:xs) (y:ys) = if x == y then pre xs ys else bf
------------------------------
 dmb ks be@(BraE ks' mp) = pre ks ks' where
  pre []     _      = be
  pre (x:xs) []     = braE ks' (adjustWith (\lmp -> dmb xs lmp) x mp)
  pre (x:xs) (y:ys) = if x == y then pre xs ys else be
------------------------------

-- | See 'Map' class method 'adjustWith''.
adjustWithListMap' :: Map map => (a -> a) -> (Key (ListMap map)) -> ListMap map a -> ListMap map a
-- N.B. One day we will have a more efficient implementation of this
adjustWithListMap' f ks0 lmp0 = dmb ks0 lmp0 where
 dmb _ Empt = Empt
------------------------------
 dmb ks bf@(BraF ks' a mp) = pre ks ks' where
  pre []     []     = let newElem = f a 
  		      in newElem `seq` BraF  ks' newElem mp
  pre []     (_:_ ) = bf
  pre (x:xs) []     = BraF ks' a (adjustWith' (\lmp -> dmb xs lmp) x mp)
  pre (x:xs) (y:ys) = if x == y then pre xs ys else bf
------------------------------
 dmb ks be@(BraE ks' mp) = pre ks ks' where
  pre []     _      = be
  pre (x:xs) []     = braE ks' (adjustWith' (\lmp -> dmb xs lmp) x mp)
  pre (x:xs) (y:ys) = if x == y then pre xs ys else be
------------------------------

-- | See 'Map' class method 'adjustMaybe'.
adjustMaybeListMap :: Map map => (a -> Maybe a) -> (Key (ListMap map)) -> ListMap map a -> ListMap map a
-- Convention below is xs is the search key list and ys is the key list fragment from the Trie (ListMap)
adjustMaybeListMap f ks0 lmp0 = dmb ks0 lmp0 where
 dmb _ Empt = Empt
------------------------------
 dmb ks bf@(BraF ks' a mp) = pre ks ks' where
  pre []     []     = case f a of Just a' -> BraF  ks' a' mp
                                  Nothing -> braE  ks'    mp
  pre []     (_:_ ) = bf
  pre (x:xs) []     = BraF ks' a (adjustMaybe (\lmp -> nonEmptyListMap (dmb xs lmp)) x mp)
  pre (x:xs) (y:ys) = if x == y then pre xs ys else bf
------------------------------
 dmb ks be@(BraE ks' mp) = pre ks ks' where
  pre []     _      = be
  pre (x:xs) []     = braE ks' (adjustMaybe (\lmp -> nonEmptyListMap (dmb xs lmp)) x mp)
  pre (x:xs) (y:ys) = if x == y then pre xs ys else be
------------------------------

-- |  See 'Map' class method 'venn'.
vennListMap ::  Map map => (a -> b -> c) -> ListMap map a -> ListMap map b -> (ListMap map a, ListMap map c, ListMap map b)
vennListMap f lmp0 lmp1 = v lmp0 lmp1 where
 appendStem ys y (BraF xs a mpx) = BraF (ys +!+ y:xs) a mpx
 appendStem ys y (BraE xs   mpx) = BraE (ys +!+ y:xs)  mpx
 appendStem _  _ Empt            = Empt
------------------------------------------
 replace k m mp = alter' (const (nonEmpty m)) k mp
------------------------------------------
 vennInner mpx mpy = (leftDiff,inter,rightDiff) 
	where 	leftDiff  = disjointUnion mpl (mapMaybe (\(l,_,_) -> nonEmpty l) mpi)
		inter     =                    mapMaybe (\(_,i,_) -> nonEmpty i) mpi
		rightDiff = disjointUnion mpr (mapMaybe (\(_,_,r) -> nonEmpty r) mpi)
		(mpl,mpi,mpr) = venn' (venn f) mpx mpy -- NB use of venn'
------------------------------------------
 v Empt lmpy    = (Empt,Empt,lmpy)
 v lmpx    Empt = (lmpx,Empt,Empt)
------------------------------------------
 v lmpx@(BraF xs0 a mpx) lmpy@(BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = (braE xs0         leftDiff
                    ,BraF xs0 (f a b) inter
                    ,braE xs0         rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraF xs a mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraF xs0 a mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraF ys0 b (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraF ys b mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraF xs0 a (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraF ys0 b mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraF xs0 a mpx) lmpy@(BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = (BraF xs0 a leftDiff
                    ,braE xs0   inter
                    ,braE xs0   rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraF xs a mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraF xs0 a mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraE ys0 (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraE ys mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraF xs0 a (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraE ys0 mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraE xs0 mpx) lmpy@(BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = (braE xs0   leftDiff
                    ,braE xs0   inter
                    ,BraF xs0 b rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraE xs mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraE xs0 mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraF ys0 b (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraF ys b mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraE xs0 (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraF ys0 b mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraE xs0 mpx) lmpy@(BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = (braE xs0 leftDiff
                    ,braE xs0 inter
                    ,braE xs0 rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraE xs mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraE xs0 mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraE ys0 (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraE ys mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraE xs0 (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraE ys0 mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------

-- |  See 'Map' class method 'venn''.
vennListMap' ::  Map map => (a -> b -> c) -> ListMap map a -> ListMap map b -> (ListMap map a, ListMap map c, ListMap map b)
vennListMap' f lmp0 lmp1 = v lmp0 lmp1 where
 appendStem ys y (BraF xs a mpx) = BraF (ys +!+ y:xs) a mpx
 appendStem ys y (BraE xs   mpx) = BraE (ys +!+ y:xs)  mpx
 appendStem _  _ Empt            = Empt
------------------------------------------
 replace k m mp = alter' (const (nonEmpty m)) k mp
------------------------------------------
 vennInner mpx mpy = (leftDiff,inter,rightDiff) 
	where 	leftDiff  = disjointUnion mpl (mapMaybe (\(l,_,_) -> nonEmpty l) mpi)
		inter     =                    mapMaybe (\(_,i,_) -> nonEmpty i) mpi
		rightDiff = disjointUnion mpr (mapMaybe (\(_,_,r) -> nonEmpty r) mpi)
		(mpl,mpi,mpr) = venn' (venn' f) mpx mpy
------------------------------------------
 v Empt lmpy    = (Empt,Empt,lmpy)
 v lmpx    Empt = (lmpx,Empt,Empt)
------------------------------------------
 v lmpx@(BraF xs0 a mpx) lmpy@(BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = (braE xs0         leftDiff
                    ,let c = f a b in c `seq` BraF xs0 c inter
                    ,braE xs0         rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraF xs a mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraF xs0 a mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraF ys0 b (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraF ys b mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraF xs0 a (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraF ys0 b mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraF xs0 a mpx) lmpy@(BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = (BraF xs0 a leftDiff
                    ,braE xs0   inter
                    ,braE xs0   rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraF xs a mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraF xs0 a mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraE ys0 (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraE ys mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraF xs0 a (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraE ys0 mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraE xs0 mpx) lmpy@(BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = (braE xs0   leftDiff
                    ,braE xs0   inter
                    ,BraF xs0 b rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraE xs mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraE xs0 mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraF ys0 b (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraF ys b mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraE xs0 (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraF ys0 b mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraE xs0 mpx) lmpy@(BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = (braE xs0 leftDiff
                    ,braE xs0 inter
                    ,braE xs0 rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraE xs mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraE xs0 mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraE ys0 (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraE ys mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraE xs0 (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraE ys0 mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------

-- |  See 'Map' class method 'vennMaybe'.
vennMaybeListMap ::  Map map => (a -> b -> Maybe c) -> ListMap map a -> ListMap map b -> (ListMap map a, ListMap map c, ListMap map b)
vennMaybeListMap f lmp0 lmp1 = v lmp0 lmp1 where
 appendStem ys y (BraF xs a mpx) = BraF (ys +!+ y:xs) a mpx
 appendStem ys y (BraE xs   mpx) = BraE (ys +!+ y:xs)  mpx
 appendStem _  _ Empt            = Empt
------------------------------------------
 replace k m mp = alter' (const (nonEmpty m)) k mp
------------------------------------------
 vennInner mpx mpy = (leftDiff,inter,rightDiff) 
	where 	leftDiff  = disjointUnion mpl (mapMaybe (\(l,_,_) -> nonEmpty l) mpi)
		inter     =                    mapMaybe (\(_,i,_) -> nonEmpty i) mpi
		rightDiff = disjointUnion mpr (mapMaybe (\(_,_,r) -> nonEmpty r) mpi)
		(mpl,mpi,mpr) = venn (vennMaybe f) mpx mpy
------------------------------------------
 v Empt lmpy    = (Empt,Empt,lmpy)
 v lmpx    Empt = (lmpx,Empt,Empt)
------------------------------------------
 v lmpx@(BraF xs0 a mpx) lmpy@(BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = (braE xs0         leftDiff
                    ,case f a b of
                    	Nothing -> braE xs0   inter
                    	Just c  -> BraF xs0 c inter
                    ,braE xs0         rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraF xs a mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraF xs0 a mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraF ys0 b (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraF ys b mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraF xs0 a (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraF ys0 b mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraF xs0 a mpx) lmpy@(BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = (BraF xs0 a leftDiff
                    ,braE xs0   inter
                    ,braE xs0   rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraF xs a mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraF xs0 a mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraE ys0 (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraE ys mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraF xs0 a (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraE ys0 mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraE xs0 mpx) lmpy@(BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = (braE xs0   leftDiff
                    ,braE xs0   inter
                    ,BraF xs0 b rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraE xs mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraE xs0 mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraF ys0 b (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraF ys b mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraE xs0 (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraF ys0 b mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------
 v lmpx@(BraE xs0 mpx) lmpy@(BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = (braE xs0 leftDiff
                    ,braE xs0 inter
                    ,braE xs0 rightDiff)
  		    where (leftDiff,inter,rightDiff) = vennInner mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpb -> case v (BraE xs mpx) lmpb of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (_,i   ,r) -> (difference 
                                                      				(BraE xs0 mpx)
                                                      				(appendStem ys0 x i)
                                                      		    ,appendStem ys0 x i
                                                      		    ,BraE ys0 (replace x r mpy))
  m []     (y:ys) = case lookup y mpx of Nothing   -> (lmpx,Empt,lmpy)
                                         Just lmpa -> case v lmpa (BraE ys mpy) of
                                                      (_,Empt,_) -> (lmpx,Empt,lmpy)
                                                      (l,i   ,_) -> (BraE xs0 (replace y l mpx)
                                                      		    ,appendStem xs0 y i
                                                      		    ,difference 
                                                      				(BraE ys0 mpy)
                                                      				(appendStem xs0 y i))
  m (x:xs) (y:ys) = if x == y then m xs ys else (lmpx,Empt,lmpy)
------------------------------------------

-- |  See 'Map' class method 'union'.
unionListMap ::  Map map => (a -> a -> a) -> ListMap map a -> ListMap map a -> ListMap map a
unionListMap f lmp0 lmp1 = u lmp0 lmp1 where
 u Empt lmp  = lmp
 u lmp  Empt = lmp
------------------------------------------
 u (BraF xs0 ax mpx) (BraF ys0 ay mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 (f ax ay) (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraF xs ax mpx) (BraF ys ay mpy))
  Sfx _ x xs     -> BraF ys0 ay (insertWith' f' x braFx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braFx lmp
                          braFx  = BraF xs ax mpx
  Sfy _ y ys     -> BraF xs0 ax (insertWith' f' y braFy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braFy
                          braFy  = BraF ys ay mpy
------------------------------------------
 u (BraF xs0 ax mpx) (BraE ys0 mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 ax (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraF xs ax mpx) (BraE ys mpy))
  Sfx _ x xs     -> BraE ys0 (insertWith' f' x braFx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braFx lmp
                          braFx  = BraF xs ax mpx
  Sfy _ y ys     -> BraF xs0 ax (insertWith' f' y braEy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braEy
                          braEy  = BraE ys mpy
------------------------------------------
 u (BraE xs0 mpx) (BraF ys0 ay mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 ay (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraE xs mpx) (BraF ys ay mpy))
  Sfx _ x xs     -> BraF ys0 ay (insertWith' f' x braEx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braEx lmp
                          braEx  = BraE xs mpx
  Sfy _ y ys     -> BraE xs0 (insertWith' f' y braFy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braFy
                          braFy  = BraF ys ay mpy
------------------------------------------
 u (BraE xs0 mpx) (BraE ys0 mpy) = case match xs0 ys0 of
  Mat            -> BraE xs0 (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraE xs mpx) (BraE ys mpy))
  Sfx _ x xs     -> BraE ys0 (insertWith' f' x braEx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braEx lmp
                          braEx  = BraE xs mpx
  Sfy _ y ys     -> BraE xs0 (insertWith' f' y braEy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braEy
                          braEy  = BraE ys mpy
------------------------------------------


-- |  See 'Map' class method 'union''.
unionListMap' ::  Map map => (a -> a -> a) -> ListMap map a -> ListMap map a -> ListMap map a
unionListMap' f lmp0 lmp1 = u lmp0 lmp1 where
 u Empt lmp  = lmp
 u lmp  Empt = lmp
------------------------------------------
 u (BraF xs0 ax mpx) (BraF ys0 ay mpy) = case match xs0 ys0 of
  Mat            -> let a = f ax ay in a `seq` BraF xs0 a (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (left `seq` right `seq` f' left right)
  		    where left = BraF xs ax mpx
  		    	  right = BraF ys ay mpy
  Sfx _ x xs     -> BraF ys0 ay (insertWith' f' x braFx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braFx lmp
                          braFx  = BraF xs ax mpx
  Sfy _ y ys     -> BraF xs0 ax (insertWith' f' y braFy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braFy
                          braFy  = BraF ys ay mpy
------------------------------------------
 u (BraF xs0 ax mpx) (BraE ys0 mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 ax (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (left `seq` f' left right)
  		    where left = BraF xs ax mpx
  		    	  right = BraE ys mpy
  Sfx _ x xs     -> BraE ys0 (insertWith' f' x braFx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braFx lmp
                          braFx  = BraF xs ax mpx
  Sfy _ y ys     -> BraF xs0 ax (insertWith' f' y braEy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braEy
                          braEy  = BraE ys mpy
------------------------------------------
 u (BraE xs0 mpx) (BraF ys0 ay mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 ay (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (right `seq` f' left right)
  		    where left = BraE xs mpx
  		    	  right = BraF ys ay mpy
  Sfx _ x xs     -> BraF ys0 ay (insertWith' f' x braEx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braEx lmp
                          braEx  = BraE xs mpx
  Sfy _ y ys     -> BraE xs0 (insertWith' f' y braFy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braFy
                          braFy  = BraF ys ay mpy
------------------------------------------
 u (BraE xs0 mpx) (BraE ys0 mpy) = case match xs0 ys0 of
  Mat            -> BraE xs0 (union' u mpx mpy) -- N.B. Use of strict union'
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraE xs mpx) (BraE ys mpy))
  Sfx _ x xs     -> BraE ys0 (insertWith' f' x braEx mpy) -- N.B. Use of strict insertWith'
                    where f' lmp = u braEx lmp
                          braEx  = BraE xs mpx
  Sfy _ y ys     -> BraE xs0 (insertWith' f' y braEy mpx) -- N.B. Use of strict insertWith'
                    where f' lmp = u lmp braEy
                          braEy  = BraE ys mpy
------------------------------------------


-- |  See 'Map' class method 'unionMaybe'.
unionMaybeListMap ::  Map map => (a -> a -> Maybe a) -> ListMap map a -> ListMap map a -> ListMap map a
unionMaybeListMap f lmp0 lmp1 = u lmp0 lmp1 where
 uNE lmpx lmpy = nonEmptyListMap (u lmpx lmpy) -- unionMaybe can yield empty maps !!
------------------------------------------
 u Empt lmp  = lmp
 u lmp  Empt = lmp
------------------------------------------
 u (BraF xs0 ax mpx) (BraF ys0 ay mpy) = case match xs0 ys0 of
  Mat            -> case f ax ay of
                    Just a  -> BraF xs0 a (unionMaybe' uNE mpx mpy)
                    Nothing -> braE xs0   (unionMaybe' uNE mpx mpy) -- N.B Use of braE, not BraE !!
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraF xs ax mpx) (BraF ys ay mpy))
  Sfx _ x xs     -> BraF ys0 ay (insertMaybe' f' x braFx mpy)
                    where f' lmp = uNE braFx lmp
                          braFx  = BraF xs ax mpx
  Sfy _ y ys     -> BraF xs0 ax (insertMaybe' f' y braFy mpx)
                    where f' lmp = uNE lmp braFy
                          braFy  = BraF ys ay mpy
------------------------------------------
 u (BraF xs0 ax mpx) (BraE ys0 mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 ax (unionMaybe' uNE mpx mpy)
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraF xs ax mpx) (BraE ys mpy))
  Sfx _ x xs     -> braE ys0 (insertMaybe' f' x braFx mpy) -- N.B Use of braE, not BraE !!
                    where f' lmp = uNE braFx lmp
                          braFx  = BraF xs ax mpx
  Sfy _ y ys     -> BraF xs0 ax (insertMaybe' f' y braEy mpx)
                    where f' lmp = uNE lmp braEy
                          braEy  = BraE ys mpy
------------------------------------------
 u (BraE xs0 mpx) (BraF ys0 ay mpy) = case match xs0 ys0 of
  Mat            -> BraF xs0 ay (unionMaybe' uNE mpx mpy)
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraE xs mpx) (BraF ys ay mpy))
  Sfx _ x xs     -> BraF ys0 ay (insertMaybe' f' x braEx mpy)
                    where f' lmp = uNE braEx lmp
                          braEx  = BraE xs mpx
  Sfy _ y ys     -> braE xs0 (insertMaybe' f' y braFy mpx) -- N.B Use of braE, not BraE !!
                    where f' lmp = uNE lmp braFy
                          braFy  = BraF ys ay mpy
------------------------------------------
 u (BraE xs0 mpx) (BraE ys0 mpy) = case match xs0 ys0 of
  Mat            -> braE xs0 (unionMaybe' uNE mpx mpy)  -- N.B Use of braE, not BraE !!
  Frk n f' xs ys -> BraE (takeN n xs0) (f' (BraE xs mpx) (BraE ys mpy))
  Sfx _ x xs     -> braE ys0 (insertMaybe' f' x braEx mpy) -- N.B Use of braE, not BraE !!
                    where f' lmp = uNE braEx lmp
                          braEx  = BraE xs mpx
  Sfy _ y ys     -> braE xs0 (insertMaybe' f' y braEy mpx) -- N.B Use of braE, not BraE !!
                    where f' lmp = uNE lmp braEy
                          braEy  = BraE ys mpy
------------------------------------------

-- |  See 'Map' class method 'intersection'.
intersectionListMap ::  Map map => (a -> b -> c) -> ListMap map a -> ListMap map b -> ListMap map c
intersectionListMap f lmp0 lmp1 = i lmp0 lmp1 where
 iNE lmpx lmpy = nonEmptyListMap (i lmpx lmpy) -- intersection can yield empty maps !!
------------------------------------------
 i Empt _    = Empt
 i _    Empt = Empt
------------------------------------------
 i (BraF xs0 a mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = BraF xs0 (f a b) (intersectionMaybe iNE mpx mpy)
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraF xs a mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraF ys b mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraF xs a mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraE ys mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraE xs0 mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraE xs mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraF ys b mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraE xs mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraE ys mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------


-- |  See 'Map' class method 'intersection''.
intersectionListMap' ::  Map map => (a -> b -> c) -> ListMap map a -> ListMap map b -> ListMap map c
intersectionListMap' f lmp0 lmp1 = i lmp0 lmp1 where
 iNE lmpx lmpy = nonEmptyListMap (i lmpx lmpy) -- intersection can yield empty maps !!
------------------------------------------
 i Empt _    = Empt
 i _    Empt = Empt
------------------------------------------
 i (BraF xs0 a mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = let c = f a b in c `seq` BraF xs0 c (intersectionMaybe iNE mpx mpy)
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraF xs a mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraF ys b mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraF xs a mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraE ys mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraE xs0 mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraE xs mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraF ys b mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraE xs mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraE ys mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------


-- |  See 'Map' class method 'intersectionMaybe'.
intersectionMaybeListMap ::  Map map => (a -> b -> Maybe c) -> ListMap map a -> ListMap map b -> ListMap map c
intersectionMaybeListMap f lmp0 lmp1 = i lmp0 lmp1 where
 iNE lmpx lmpy = nonEmptyListMap (i lmpx lmpy) -- intersection can yield empty maps !!
------------------------------------------
 i Empt _    = Empt
 i _    Empt = Empt
------------------------------------------
 i (BraF xs0 a mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = case f a b of
                    Just c  -> BraF xs0 c (intersectionMaybe' iNE mpx mpy)
                    Nothing -> braE xs0   (intersectionMaybe' iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraF xs a mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraF ys b mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe' iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraF xs a mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraE ys mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraE xs0 mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe' iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraE xs mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraF ys b mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------
 i (BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (intersectionMaybe' iNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> Empt
                                         Just lmpb -> case i (BraE xs mpx) lmpb of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (ys0 +!+ x:zs) c mpz
                                                      BraE zs   mpz -> BraE (ys0 +!+ x:zs)   mpz
  m []     (y:ys) = case lookup y mpx of Nothing   -> Empt
                                         Just lmpa -> case i lmpa (BraE ys mpy) of
                                                      Empt          -> Empt
                                                      BraF zs c mpz -> BraF (xs0 +!+ y:zs) c mpz
                                                      BraE zs   mpz -> BraE (xs0 +!+ y:zs)   mpz
  m (x:xs) (y:ys) = if x == y then m xs ys else Empt
------------------------------------------

-- | See 'Map' class method 'difference'.
differenceListMap :: Map map => ListMap map a -> ListMap map b -> ListMap map a
differenceListMap lmp0 lmp1 = d lmp0 lmp1 where
 dNE lmpx lmpy = nonEmptyListMap (d lmpx lmpy) -- difference can yield empty maps !!
------------------------------------------
 d Empt _    = Empt
 d lmpx Empt = lmpx
------------------------------------------
 d lmpx@(BraF xs0 a mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (differenceMaybe' dNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraF xs a mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = BraF xs0 a (adjustMaybe' (\lmpa -> dNE lmpa (BraF ys b mpy)) y mpx)
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------
 d lmpx@(BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = BraF xs0 a (differenceMaybe' dNE mpx mpy)
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraF xs a mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = BraF xs0 a (adjustMaybe' (\lmpa -> dNE lmpa (BraE ys mpy)) y mpx)
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------
 d lmpx@(BraE xs0 mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (differenceMaybe' dNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraE xs mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = braE xs0 (adjustMaybe' (\lmpa -> dNE lmpa (BraF ys b mpy)) y mpx) -- Note use of braE!
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------
 d lmpx@(BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (differenceMaybe' dNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraE xs mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = braE xs0 (adjustMaybe' (\lmpa -> dNE lmpa (BraE ys mpy)) y mpx) -- Note use of braE!
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------


-- | See 'Map' class method 'differenceMaybe'.
differenceMaybeListMap :: Map map => (a -> b -> Maybe a) -> ListMap map a -> ListMap map b -> ListMap map a
differenceMaybeListMap f lmp0 lmp1 = d lmp0 lmp1 where
 dNE lmpx lmpy = nonEmptyListMap (d lmpx lmpy) -- difference can yield empty maps !!
------------------------------------------
 d Empt _    = Empt
 d lmpx Empt = lmpx
------------------------------------------
 d lmpx@(BraF xs0 a mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = case f a b of
                    Nothing -> braE xs0    (differenceMaybe' dNE mpx mpy) -- Note use of braE!
                    Just a' -> BraF xs0 a' (differenceMaybe' dNE mpx mpy)
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraF xs a mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = BraF xs0 a (adjustMaybe' (\lmpa -> dNE lmpa (BraF ys b mpy)) y mpx)
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------
 d lmpx@(BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = BraF xs0 a (differenceMaybe' dNE mpx mpy)
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraF xs a mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = BraF xs0 a (adjustMaybe' (\lmpa -> dNE lmpa (BraE ys mpy)) y mpx)
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------
 d lmpx@(BraE xs0 mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (differenceMaybe' dNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraE xs mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = braE xs0 (adjustMaybe' (\lmpa -> dNE lmpa (BraF ys b mpy)) y mpx) -- Note use of braE!
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------
 d lmpx@(BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = braE xs0 (differenceMaybe' dNE mpx mpy) -- Note use of braE!
  m (x:xs) []     = case lookup x mpy of Nothing   -> lmpx
                                         Just lmpb -> case d (BraE xs mpx) lmpb of
                                                      Empt           -> Empt
                                                      BraF zs a' mpz -> BraF (ys0 +!+ x:zs) a' mpz
                                                      BraE zs    mpz -> BraE (ys0 +!+ x:zs)    mpz
  m []     (y:ys) = braE xs0 (adjustMaybe' (\lmpa -> dNE lmpa (BraE ys mpy)) y mpx) -- Note use of braE!
  m (x:xs) (y:ys) = if x==y then m xs ys else lmpx
------------------------------------------

-- | See 'Map' class method 'isSubsetOf'.
isSubsetOfListMap :: Map map => ListMap map a -> ListMap map b -> Bool
-- This is basically finding out if (differenceListMap lmp0 lmp1 == Empt)
-- If so, lmp0 is a submap of lmp1.
------------------------------------------
isSubsetOfListMap Empt _    = True
isSubsetOfListMap _    Empt = False 
------------------------------------------
isSubsetOfListMap (BraF xs0 a mpx) (BraF ys0 _ mpy) = m xs0 ys0 where
  m []     []     = isSubmapOf isSubsetOfListMap mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> isSubsetOfListMap (BraF xs a mpx) lmpb
  m []     (_:_ ) = False
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------
isSubsetOfListMap (BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = False
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> isSubsetOfListMap (BraF xs a mpx) lmpb
  m []     (_:_ ) = False
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------
isSubsetOfListMap (BraE xs0 mpx) (BraF ys0 _ mpy) = m xs0 ys0 where
  m []     []     = isSubmapOf isSubsetOfListMap mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> isSubsetOfListMap (BraE xs mpx) lmpb
  m []     (_:_ ) = False -- mpx must contain at least 2 entries
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------
isSubsetOfListMap (BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = isSubmapOf isSubsetOfListMap mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> isSubsetOfListMap (BraE xs mpx) lmpb
  m []     (_:_ ) = False -- mpx must contain at least 2 entries
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------


-- | See 'Map' class method 'isSubmapOf'.
isSubmapOfListMap :: Map map => (a -> b -> Bool) -> ListMap map a -> ListMap map b -> Bool
isSubmapOfListMap p lmp0 lmp1 = d lmp0 lmp1 where
------------------------------------------
 d Empt _    = True
 d _    Empt = False
------------------------------------------
 d (BraF xs0 a mpx) (BraF ys0 b mpy) = m xs0 ys0 where
  m []     []     = if p a b then isSubmapOf d mpx mpy else False
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> d (BraF xs a mpx) lmpb
  m []     (_:_ ) = False
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------
 d (BraF xs0 a mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = False
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> d (BraF xs a mpx) lmpb
  m []     (_:_ ) = False
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------
 d (BraE xs0 mpx) (BraF ys0 _ mpy) = m xs0 ys0 where
  m []     []     = isSubmapOf d mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> d (BraE xs mpx) lmpb
  m []     (_:_ ) = False -- mpx must contain at least 2 entries
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------
 d (BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = isSubmapOf d mpx mpy
  m (x:xs) []     = case lookup x mpy of Nothing   -> False
                                         Just lmpb -> d (BraE xs mpx) lmpb
  m []     (_:_ ) = False -- mpx must contain at least 2 entries
  m (x:xs) (y:ys) = if x==y then m xs ys else False
------------------------------------------

-- | See 'Map' class method 'alter'.
alterListMap :: Map map => (Maybe a -> Maybe a) -> (Key (ListMap map)) -> ListMap map a -> ListMap map a
-- Convention below is xs is the search key list and ys is the key list fragment from the Trie (ListMap)
alterListMap f xs0 lmp0 = iw xs0 lmp0 where
 iwNE xs (Just lmp) = nonEmptyListMap (iw xs lmp) -- alter can yield empty maps !!
 iwNE xs  Nothing   = nonEmptyListMap (iw xs empty)
------------------------------
 iw xs Empt = case (f Nothing) of
 		Just ax 	-> BraF xs ax empty
 		Nothing		-> Empt
------------------------------
 iw xs m@(BraF ys ay mp) = case match xs ys of
   Mat              -> case (f (Just ay)) of   -- xs == ys
                        Just ax -> BraF ys ax mp
                        Nothing -> braE ys    mp -- N.B. Use of braE, not BraE
   Frk n f' xs' ys' -> case (f Nothing) of
   			Just ax -> BraE (takeN n ys) (f' (BraF xs' ax empty) (BraF ys' ay mp))
   			Nothing -> m
   Sfy _ y' ys'     -> case (f Nothing) of
   			Just ax -> BraF xs ax (singleton y' (BraF ys' ay mp))
   			Nothing -> m
   Sfx _ x' xs'     -> BraF ys ay (alter (iwNE xs') x' mp)
------------------------------
 iw xs m@(BraE ys mp) = case match xs ys of
   Mat              -> case (f Nothing) of
   			Just ax -> BraF ys ax mp   -- xs == ys
   			Nothing -> m
   Frk n f' xs' ys' -> case (f Nothing) of
   			Just ax -> BraE (takeN n ys) (f' (BraF xs' ax empty) (BraE ys' mp))
   			Nothing -> m
   Sfy _ y' ys'     -> case (f Nothing) of
   			Just ax -> BraF xs ax (singleton y' (BraE ys' mp))
   			Nothing -> m
   Sfx _ x' xs'     -> braE ys (alter (iwNE xs') x' mp)  -- N.B. Use of braE, not BraE
------------------------------

-- | See 'Map' class method 'insertWith'.
insertWithListMap :: Map map => (a -> a) -> (Key (ListMap map)) -> a -> ListMap map a -> ListMap map a
-- Convention below is xs is the search key list and ys is the key list fragment from the Trie (ListMap)
-- N.B We always use the Strict insertWith' method here!
insertWithListMap f xs0 ax lmp0 = iw xs0 lmp0 where
 iw xs Empt = BraF xs ax empty
------------------------------
 iw xs (BraF ys ay mp) = case match xs ys of
   Mat              -> BraF ys (f ay) mp  -- xs == ys
   Frk n f' xs' ys' -> BraE (takeN n ys) (f' (BraF xs' ax empty) (BraF ys' ay mp))
   Sfy _ y' ys'     -> BraF xs ax (singleton y' (BraF ys' ay mp))
   Sfx _ x' xs'     -> BraF ys ay (insertWith' (iw xs') x' (BraF xs' ax empty) mp)
------------------------------
 iw xs (BraE ys mp) = case match xs ys of
   Mat              -> BraF ys ax mp   -- xs == ys
   Frk n f' xs' ys' -> BraE (takeN n ys) (f' (BraF xs' ax empty) (BraE ys' mp))
   Sfy _ y' ys'     -> BraF xs ax (singleton y' (BraE ys' mp))
   Sfx _ x' xs'     -> BraE ys (insertWith' (iw xs') x' (BraF xs' ax empty) mp)
------------------------------

-- | See 'Map' class method 'insertWith'''.
insertWithListMap' :: Map map => (a -> a) -> (Key (ListMap map)) -> a -> ListMap map a -> ListMap map a
-- Convention below is xs is the search key list and ys is the key list fragment from the Trie (ListMap)
-- N.B We always use the Stricter insertWith'' method here!
insertWithListMap' f xs0 ax lmp0 = iw xs0 lmp0 where
 iw xs Empt = ax `seq` BraF xs ax empty
------------------------------
 iw xs (BraF ys ay mp) = case match xs ys of
   Mat              -> let ay' = f ay in ay' `seq` BraF ys ay' mp  -- xs == ys
   Frk n f' xs' ys' -> ax `seq` BraE (takeN n ys) (f' (BraF xs' ax empty) (BraF ys' ay mp))
   Sfy _ y' ys'     -> ax `seq` BraF xs ax (singleton y' (BraF ys' ay mp))
   Sfx _ x' xs'     -> BraF ys ay (insertWith' (iw xs') x' (ax `seq` (BraF xs' ax empty)) mp) -- N.B.!!
------------------------------
 iw xs (BraE ys mp) = case match xs ys of
   Mat              -> ax `seq` BraF ys ax mp   -- xs == ys
   Frk n f' xs' ys' -> ax `seq` BraE (takeN n ys) (f' (BraF xs' ax empty) (BraE ys' mp))
   Sfy _ y' ys'     -> ax `seq` BraF xs ax (singleton y' (BraE ys' mp))
   Sfx _ x' xs'     -> BraE ys (insertWith' (iw xs') x' (ax `seq` (BraF xs' ax empty)) mp) -- N.B.!!
------------------------------


-- | See 'Map' class method 'insertMaybe'.
insertMaybeListMap :: Map map => (a -> Maybe a) -> (Key (ListMap map)) -> a -> ListMap map a -> ListMap map a
-- Convention below is xs is the search key list and ys is the key list fragment from the Trie (ListMap)
insertMaybeListMap f xs0 ax lmp0 = iw xs0 lmp0 where
 iwNE xs lmp = nonEmptyListMap (iw xs lmp) -- insertMaybe can yield empty maps !!
------------------------------
 iw xs Empt = BraF xs ax empty
------------------------------
 iw xs (BraF ys ay mp) = case match xs ys of
   Mat              -> case f ay of   -- xs == ys
                       Just ay' -> BraF ys ay' mp
                       Nothing  -> braE ys     mp -- N.B. Use of braE, not BraE
   Frk n f' xs' ys' -> BraE (takeN n ys) (f' (BraF xs' ax empty) (BraF ys' ay mp))
   Sfy _ y' ys'     -> BraF xs ax (singleton y' (BraF ys' ay mp))
   Sfx _ x' xs'     -> BraF ys ay (insertMaybe (iwNE xs') x' (BraF xs' ax empty) mp)
------------------------------
 iw xs (BraE ys mp) = case match xs ys of
   Mat              -> BraF ys ax mp   -- xs == ys
   Frk n f' xs' ys' -> BraE (takeN n ys) (f' (BraF xs' ax empty) (BraE ys' mp))
   Sfy _ y' ys'     -> BraF xs ax (singleton y' (BraE ys' mp))
   Sfx _ x' xs'     -> braE ys (insertMaybe (iwNE xs') x' (BraF xs' ax empty) mp)  -- N.B. Use of braE, not BraE
------------------------------

-- | See 'Map' class method 'foldElems'.
foldElemsListMap :: Map map => (a -> b -> b) -> b -> ListMap map a -> b
foldElemsListMap f b0 lmp0  = fld lmp0 b0 where
 fld  Empt         b = b
 fld (BraF _ a mp) b = f a (foldElems fld b mp)
 fld (BraE _   mp) b =      foldElems fld b mp

-- | See 'Map' class method 'foldKeys'.
foldKeysListMap :: Map map => ((Key (ListMap map)) -> b -> b) -> b -> ListMap map a -> b
foldKeysListMap f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks _ mp) b = f (revTo rks ks) (foldAssocs f' b mp)
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
 fld rks (BraE ks   mp) b = foldAssocs f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldAssocs'.
foldAssocsListMap :: Map map => ((Key (ListMap map)) -> a -> b -> b) -> b -> ListMap map a -> b
foldAssocsListMap f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks a mp) b = f (revTo rks ks) a (foldAssocs f' b mp)
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
 fld rks (BraE ks   mp) b = foldAssocs f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldElems''.
foldElemsListMap' :: Map map => (a -> b -> b) -> b -> ListMap map a -> b
foldElemsListMap' f b0 lmp0 = fld lmp0 b0 where
 fld  Empt         b = b
 fld (BraF _ a mp) b = let b' = foldElems' fld b mp  in b' `seq` f a b'
 fld (BraE _   mp) b =          foldElems' fld b mp

-- | See 'Map' class method 'foldKeys''.
foldKeysListMap' :: Map map => ((Key (ListMap map)) -> b -> b) -> b -> ListMap map a -> b
foldKeysListMap' f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks _ mp) b = b'' `seq` f (revTo rks ks) b''
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
                                  b''         = foldAssocs' f' b mp
 fld rks (BraE ks   mp) b = foldAssocs' f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldAssocs''.
foldAssocsListMap' :: Map map => ((Key (ListMap map)) -> a -> b -> b) -> b -> ListMap map a -> b
foldAssocsListMap' f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks a mp) b = b'' `seq` f (revTo rks ks) a b''
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
                                  b''         = foldAssocs' f' b mp
 fld rks (BraE ks   mp) b = foldAssocs' f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

------------------------------------------------------------------------------------------

-- Group an ordered list of assocs according to which part of the map they will form
clump :: (Eq a) => [([a], b)] -> [a] -> ([b], [(a, [([a], b)])])
clump as prefix = 
	if 	null nonNulls
	then	(L.map snd nulls, [])
	else	(L.map snd nulls, clumps' [(k',c' [])])
	-- 'currentClump' and 'clumps' are list building continuations to preserve order of 'as'
	where 	f (currentKey,currentClump,clumps) (key,tl) =
			if 	key == currentKey
			then	(currentKey,  currentClump . (tl:),  clumps                                   )
			else	(key,        (tl:),                  clumps . ((currentKey,currentClump []):) )
		(nulls,nonNulls) = L.partition (null . fst) $ L.map (\(k,a) -> (fromJust $ L.stripPrefix prefix k,a)) as
		rest = L.map (\(k:ks,a) -> (k,(ks,a))) nonNulls
		(k',c',clumps') = L.foldl' f (fst $ head rest,id,id) rest
		
commonPrefix :: (Eq a) => [([a], b)] -> [a]
commonPrefix as = common (fst $ head as) (fst $ last as)
	where 	common [] _ = []
		common _ [] = []
		common (ka:kas) (kb:kbs) =
			if 	ka == kb
			then	ka : common kas kbs
			else	[]
	
fromAssocsAscWithListMap :: OrderedMap map => (a -> a -> a) -> [([k],a)] -> ListMap map a
fromAssocsAscWithListMap _ [] = emptyListMap
fromAssocsAscWithListMap f as = 
	case nulls of
		[]	-> braE prefix                     (fromAssocsAsc innerAs) 
		_	-> BraF prefix (L.foldl1' f nulls) (fromAssocsAsc innerAs) 
	where	(nulls,clumps) = clump as prefix
		prefix = commonPrefix as
		innerAs = L.map (\(k,as') -> (k,fromAssocsAscWith f as')) clumps -- NB Shouldnt have any repeated keys in 'innerAs' if 'as' is ordered

fromAssocsDescWithListMap :: OrderedMap map => (a -> a -> a) -> [([k],a)] -> ListMap map a
fromAssocsDescWithListMap _ [] = emptyListMap
fromAssocsDescWithListMap f as = 
	case nulls of
		[]	-> braE prefix                     (fromAssocsDesc innerAs) 
		_	-> BraF prefix (L.foldl1' f nulls) (fromAssocsDesc innerAs) 
	where	(nulls,clumps) = clump as prefix
		prefix = commonPrefix as
		innerAs = L.map (\(k,as') -> (k,fromAssocsDescWith f as')) clumps -- NB Shouldnt have any repeated keys in 'innerAs' if 'as' is ordered
		
fromAssocsAscMaybeListMap :: OrderedMap map => (a -> a -> Maybe a) -> [([k],a)] -> ListMap map a
fromAssocsAscMaybeListMap _ [] = emptyListMap
fromAssocsAscMaybeListMap f as = 
	case L.foldl' insNull Nothing nulls of
		Nothing	-> braE prefix   (fromAssocsAsc innerAs) 
		Just a	-> BraF prefix a (fromAssocsAsc innerAs) 
	where	insNull Nothing  b = Just b
		insNull (Just a) b = f a b
		(nulls,clumps) = clump as prefix
		prefix = commonPrefix as
		innerAs = catMaybes $ L.map (\(k,as') -> do mp <- nonEmpty $ fromAssocsAscMaybe f as'; return (k,mp)) clumps
		 -- NB Shouldnt have any repeated keys in 'innerAs' if 'as' is ordered

fromAssocsDescMaybeListMap :: OrderedMap map => (a -> a -> Maybe a) -> [([k],a)] -> ListMap map a
fromAssocsDescMaybeListMap _ [] = emptyListMap
fromAssocsDescMaybeListMap f as = 
	case L.foldl' insNull Nothing nulls of
		Nothing	-> braE prefix   (fromAssocsDesc innerAs)
		Just a	-> BraF prefix a (fromAssocsDesc innerAs)
	where	insNull Nothing  b = Just b
		insNull (Just a) b = f a b
		(nulls,clumps) = clump as prefix
		prefix = commonPrefix as
		innerAs = catMaybes $ L.map (\(k,as') -> do mp <- nonEmpty $ fromAssocsDescMaybe f as'; return (k,mp)) clumps
		 -- NB Shouldnt have any repeated keys in 'innerAs' if 'as' is ordered

-- | See 'Map' class method 'foldElemsAsc'.
foldElemsAscListMap :: OrderedMap map => (a -> b -> b) -> b -> ListMap map a -> b
foldElemsAscListMap f b0 lmp0  = fld lmp0 b0 where
 fld  Empt         b = b
 fld (BraF _ a mp) b = f a (foldElemsAsc fld b mp)
 fld (BraE _   mp) b =      foldElemsAsc fld b mp

-- | See 'Map' class method 'foldElemsDesc'.
foldElemsDescListMap :: OrderedMap map => (a -> b -> b) -> b -> ListMap map a -> b
foldElemsDescListMap f b0 lmp0 = fld lmp0 b0 where
 fld  Empt         b = b
 fld (BraF _ a mp) b = foldElemsDesc fld (f a b) mp
 fld (BraE _   mp) b = foldElemsDesc fld b       mp

-- | See 'Map' class method 'foldKeysAsc'.
foldKeysAscListMap :: OrderedMap map => ((Key (ListMap map)) -> b -> b) -> b -> ListMap map a -> b
foldKeysAscListMap f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks _ mp) b = f (revTo rks ks) (foldAssocsAsc f' b mp)
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
 fld rks (BraE ks   mp) b = foldAssocsAsc f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldKeysDesc'.
foldKeysDescListMap :: OrderedMap map => ((Key (ListMap map)) -> b -> b) -> b -> ListMap map a -> b
foldKeysDescListMap f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks _ mp) b = foldAssocsDesc f' (f (revTo rks ks) b) mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
 fld rks (BraE ks   mp) b = foldAssocsDesc f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldAssocsAsc'.
foldAssocsAscListMap :: OrderedMap map => ((Key (ListMap map)) -> a -> b -> b) -> b -> ListMap map a -> b
foldAssocsAscListMap f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks a mp) b = f (revTo rks ks) a (foldAssocsAsc f' b mp)
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
 fld rks (BraE ks   mp) b = foldAssocsAsc f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldAssocsDesc'.
foldAssocsDescListMap :: OrderedMap map => ((Key (ListMap map)) -> a -> b -> b) -> b -> ListMap map a -> b
foldAssocsDescListMap f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks a mp) b = foldAssocsDesc f' (f (revTo rks ks) a b) mp 
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
 fld rks (BraE ks   mp) b = foldAssocsDesc f' b mp 
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldElemsAsc''.
foldElemsAscListMap' :: OrderedMap map => (a -> b -> b) -> b -> ListMap map a -> b
foldElemsAscListMap' f b0 lmp0 = fld lmp0 b0 where
 fld  Empt         b = b
 fld (BraF _ a mp) b = let b' = foldElemsAsc' fld b mp  in b' `seq` f a b'
 fld (BraE _   mp) b =          foldElemsAsc' fld b mp

-- | See 'Map' class method 'foldElemsDesc''.
foldElemsDescListMap' :: OrderedMap map => (a -> b -> b) -> b -> ListMap map a -> b
foldElemsDescListMap' f b0 lmp0 = fld lmp0 b0 where
 fld  Empt         b = b
 fld (BraF _ a mp) b = let b' = f a b in b' `seq` foldElemsDesc' fld b' mp
 fld (BraE _   mp) b =                            foldElemsDesc' fld b  mp

-- | See 'Map' class method 'foldKeysAsc''.
foldKeysAscListMap' :: OrderedMap map => ((Key (ListMap map)) -> b -> b) -> b -> ListMap map a -> b
foldKeysAscListMap' f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks _ mp) b = b'' `seq` f (revTo rks ks) b''
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
                                  b''         = foldAssocsAsc' f' b mp
 fld rks (BraE ks   mp) b = foldAssocsAsc' f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldKeysDesc''.
foldKeysDescListMap' :: OrderedMap map => ((Key (ListMap map)) -> b -> b) -> b -> ListMap map a -> b
foldKeysDescListMap' f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks _ mp) b = b'' `seq` foldAssocsDesc' f' b'' mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
                                  b''         = f (revTo rks ks) b
 fld rks (BraE ks   mp) b = foldAssocsDesc' f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldAssocsAsc''.
foldAssocsAscListMap' :: OrderedMap map => ((Key (ListMap map)) -> a -> b -> b) -> b -> ListMap map a -> b
foldAssocsAscListMap' f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks a mp) b = b'' `seq` f (revTo rks ks) a b''
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
                                  b''         = foldAssocsAsc' f' b mp
 fld rks (BraE ks   mp) b = foldAssocsAsc' f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldAssocsDesc''.
foldAssocsDescListMap' :: OrderedMap map => ((Key (ListMap map)) -> a -> b -> b) -> b -> ListMap map a -> b
foldAssocsDescListMap' f b0 lmp0 = fld [] lmp0 b0 where
 fld _    Empt          b = b
 fld rks (BraF ks a mp) b = b'' `seq` foldAssocsDesc' f' b'' mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'
                                  b''         = f (revTo rks ks) a b
 fld rks (BraE ks   mp) b = foldAssocsDesc' f' b mp
                            where f' k lmp b' = fld (k : revTo ks rks) lmp b'

-- | See 'Map' class method 'foldElemsUInt'.
foldElemsUIntListMap :: Map map => (a -> Int# -> Int#) -> Int# -> ListMap map a -> Int#
foldElemsUIntListMap f n0 lmp0 = fld lmp0 n0 where
 fld  Empt         n = n
 fld (BraF _ a mp) n = foldElemsUInt fld (f a n) mp
 fld (BraE _   mp) n = foldElemsUInt fld n mp

-- | See 'Map' class method 'map'.
mapListMap :: Map map => (a -> b) -> ListMap map a -> ListMap map b
mapListMap _  Empt          = Empt
mapListMap f (BraF ks a mp) = BraF ks (f a) (map' (mapListMap f) mp) -- Note use of strict map'
mapListMap f (BraE ks   mp) = BraE ks       (map' (mapListMap f) mp) -- Note use of strict map'

-- | See 'Map' class method 'map''.
mapListMap' :: Map map => (a -> b) -> ListMap map a -> ListMap map b
mapListMap' _  Empt          = Empt
mapListMap' f (BraF ks a mp) = let b = f a in b `seq` BraF ks b (map' (mapListMap' f) mp) -- Note use of strict map'
mapListMap' f (BraE ks   mp) =                        BraE ks   (map' (mapListMap' f) mp) -- Note use of strict map'

-- | See 'Map' class method 'mapMaybe'.
mapMaybeListMap :: Map map => (a -> Maybe b) -> ListMap map a -> ListMap map b
mapMaybeListMap _  Empt          = Empt
mapMaybeListMap f (BraF ks a mp) = let mp' = mapMaybe (\lmp -> nonEmptyListMap (mapMaybeListMap f lmp)) mp
                                  in case f a of Just b  -> BraF ks b mp'
                                                 Nothing -> braE ks   mp'
mapMaybeListMap f (BraE ks   mp) = let mp' = mapMaybe (\lmp -> nonEmptyListMap (mapMaybeListMap f lmp)) mp
                                  in braE ks mp'

-- | See 'Map' class method 'mapWithKey'.
mapWithKeyListMap :: Map map => ((Key (ListMap map)) -> a -> b) -> ListMap map a -> ListMap map b
mapWithKeyListMap f mp = mwk id mp where
 mwk _    Empt           = Empt
 mwk kcont (BraF ks a mp') = BraF ks (f (kcont ks) a) (mapWithKey' f' mp') -- Note use of strict mapWithKey'
                           where f' k lmp = mwk (kcont . (ks++) . (k:)) lmp
 mwk kcont (BraE ks   mp') = BraE ks (mapWithKey' f' mp') -- Note use of strict mapWithKey'
                           where f' k lmp = mwk (kcont . (ks++) . (k:)) lmp

-- | See 'Map' class method 'mapWithKey''.
mapWithKeyListMap' :: Map map => ((Key (ListMap map)) -> a -> b) -> ListMap map a -> ListMap map b
mapWithKeyListMap' f mp = mwk id mp where
 mwk _    Empt           = Empt
 mwk kcont (BraF ks a mp') = let b = f (kcont ks) a
                           in  b `seq` BraF ks b (mapWithKey' f' mp') -- Note use of strict mapWithKey'
                           where f' k lmp = mwk (kcont . (ks++) . (k:)) lmp
 mwk kcont (BraE ks   mp') = BraE ks (mapWithKey' f' mp') -- Note use of strict mapWithKey'
                           where f' k lmp = mwk (kcont . (ks++) . (k:)) lmp

-- | See 'Map' class method 'mapMaybe'.
filterListMap :: Map map => (a -> Bool) -> ListMap map a -> ListMap map a
filterListMap p lmp0 = flt lmp0 where
 flt     Empt          = Empt
 flt    (BraF ks a mp) = let mp' = mapMaybe (\lmp -> nonEmptyListMap (flt lmp)) mp
                         in if p a then BraF ks a mp'
                                   else braE ks   mp'
 flt    (BraE ks   mp) = let mp' = mapMaybe (\lmp -> nonEmptyListMap (flt lmp)) mp
                         in braE ks mp'


-- | See 'Map' class method 'valid'.
validListMap :: Map map => ListMap map a -> Maybe String
validListMap  Empt = Nothing
validListMap  lmp  = validListMap' lmp
-- Disallows Empt
validListMap' :: Map map => ListMap map a -> Maybe String
validListMap'  Empt         = Just "ListMap: Non-empty map contains Empt node."
-- Empty and singleton sub-maps are OK
validListMap' (BraF _ _ mp) = case valid mp of
                             Nothing -> foldElems valAccum Nothing mp
                             Just s  -> Just ("ListMap:" ++ s)
-- Empty and singleton sub-maps are invalid
validListMap' (BraE _   mp) = case valid mp of
                             Nothing -> case status mp of
                                        None    -> Just ("ListMap: Empty branch map in BraE node.")
                                        One _ _ -> Just ("ListMap: Singleton branch map in BraE node.")
                                        Many    -> foldElems valAccum Nothing mp
                             Just s  -> Just ("ListMap:" ++ s)
-- Accumulating valid (does not accept empty ListMaps)
valAccum :: Map map => ListMap map a -> Maybe String -> Maybe String
valAccum lmp Nothing = validListMap' lmp
valAccum _   just    = just

-- | See 'Map' class method 'compareKey.
compareKeyListMap :: OrderedMap map => ListMap map a -> (Key (ListMap map)) -> (Key (ListMap map)) -> Ordering
compareKeyListMap _  []     []     = EQ
compareKeyListMap _  _      []     = GT
compareKeyListMap _  []     _      = LT
compareKeyListMap mp (x:xs) (y:ys) = 
	case (compareKey (innerMap mp) x y) of
		GT -> GT
		EQ -> compareKeyListMap mp xs ys
		LT -> LT
	where 	innerMap :: ListMap map a -> map a
		innerMap _ = undefined

--------------------------------------------------------------------------
--                         OTHER INSTANCES                              --
--------------------------------------------------------------------------
{-
--------
-- Eq --
--------
-- Needs -fallow-undecidable-instances
instance (Eq (Key map), Eq a, Eq (map (ListMap map a))) => Eq (ListMap map a) where
 Empt            == Empt            = True
 BraF ks0 a0 mp0 == BraF ks1 a1 mp1 = (ks0==ks1) && (a0==a1) && (mp0==mp1)
 BraE ks0    mp0 == BraE ks1    mp1 = (ks0==ks1) && (mp0==mp1)
 _               == _               = False

---------
-- Ord --
---------
-- Needs -fallow-undecidable-instances
instance (Map map, Ord (Key map), Ord a, Ord (map (ListMap map a))) => Ord (ListMap map a) where
 compare Empt Empt = EQ
 compare Empt _    = LT
 compare _    Empt = GT
-----------------------
 compare (BraF xs0 ax mpx) (BraF ys0 ay mpy) = m xs0 ys0 where
  m []     []     = case compare ax ay of
                    LT -> LT
                    EQ -> compare mpx mpy
                    GT -> GT
  m (_:_ ) []     = GT
  m []     (_:_ ) = LT
  m (x:xs) (y:ys) = case compare x y of
                    LT -> LT
                    EQ -> m xs ys
                    GT -> GT
-----------------------
 compare (BraF xs0 ax mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     _      = LT
  m (x:xs) []     = let sx = singleton x (BraF xs ax mpx) in sx `seq` compare sx mpy
  m (x:xs) (y:ys) = case compare x y of
                    LT -> LT
                    EQ -> m xs ys
                    GT -> GT
-----------------------
 compare (BraE xs0 mpx) (BraF ys0 ay mpy) = m xs0 ys0 where
  m _      []     = GT
  m []     (y:ys) = let sy = singleton y (BraF ys ay mpy) in sy `seq` compare mpx sy
  m (x:xs) (y:ys) = case compare x y of
                    LT -> LT
                    EQ -> m xs ys
                    GT -> GT
-----------------------
 compare (BraE xs0 mpx) (BraE ys0 mpy) = m xs0 ys0 where
  m []     []     = compare mpx mpy
  m (x:xs) []     = let sx = singleton x (BraE xs mpx) in sx `seq` compare sx mpy
  m []     (y:ys) = let sy = singleton y (BraE ys mpy) in sy `seq` compare mpx sy
  m (x:xs) (y:ys) = case compare x y of
                    LT -> LT
                    EQ -> m xs ys
                    GT -> GT
-----------------------

----------
-- Show --
----------
instance (Map map, Show (Key map), Show a) => Show (ListMap map a) where
  showsPrec d mp  = showParen (d > 10) $
    showString "fromAssocs " . shows (assocs mp)

----------
-- Read --
----------
instance (Map map, R.Read (Key map), R.Read a) => R.Read (ListMap map a) where
 readPrec = R.parens $ R.prec 10 $ do R.Ident "fromAssocs" <- R.lexP
                                      xs <- R.readPrec
                                      return (fromAssocs xs)
 readListPrec = R.readListPrecDefault

------------------------
-- Typeable/Typeable1 --
------------------------
instance (Typeable1 map,Typeable (Key map)) => Typeable1 (ListMap map) where
 typeOf1 mp = mkTyConApp (mkTyCon "Data.GMap.ListMap.ListMap") [typeOf1 m, typeOf k]
  where BraF [k] _ m = mp -- This is just to get types for k & m !!
--------------
instance (Typeable1 (ListMap map), Typeable a) => Typeable (ListMap map a) where
 typeOf = typeOfDefault

-------------
-- Functor --
-------------
instance Map map => Functor (ListMap map) where
-- fmap :: (a -> b) -> ListMap map a -> ListMap map b
   fmap = mapListMap -- The lazy version

-----------------
-- Data.Monoid --
-----------------
instance (Map map, M.Monoid a) => M.Monoid (ListMap map a) where
-- mempty :: ListMap map a
   mempty = emptyListMap
-- mappend :: ListMap map a -> ListMap map a -> ListMap map a
   mappend map0 map1 = unionListMap M.mappend map0 map1
-- mconcat :: [ListMap map a] -> ListMap map a
   mconcat maps = L.foldr (unionListMap M.mappend) emptyListMap maps

-------------------
-- Data.Foldable --
-------------------
instance Map map => F.Foldable (ListMap map) where
-- fold :: Monoid m => ListMap map m -> m
   fold mp = foldElemsListMap M.mappend M.mempty mp
-- foldMap :: Monoid m => (a -> m) -> ListMap map a -> m
   foldMap f mp = foldElemsListMap (\a b -> M.mappend (f a) b) M.mempty mp
-- foldr :: (a -> b -> b) -> b -> ListMap map a -> b
   foldr f b0 mp = foldElemsListMap f b0 mp
-- foldl :: (a -> b -> a) -> a -> ListMap map b -> a
   foldl f b0 mp = foldElemsListMap (flip f) b0 mp
{- ToDo: Implement properly. Meantime Foldable class has suitable defaults via lists.
-- foldr1 :: (a -> a -> a) -> ListMap map a -> a
   foldr1 = undefined
-- foldl1 :: (a -> a -> a) -> ListMap map a -> a
   foldl1 = undefined
-}
-}
