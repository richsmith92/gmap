{-# OPTIONS_GHC -fglasgow-exts -XNoMonomorphismRestriction -Wall -fno-warn-missing-signatures #-}

module Data.GMap.AssocList where

import Data.GMap 
import qualified Data.List as L
import Data.Maybe(catMaybes,isNothing)
import Data.Ord
import GHC.Base

-- Unsorted assoc list with no duplicate keys
newtype AList k a = AL [(k,a)]

keyEq a b = (fst a) == (fst b)
keysOf = L.map fst
elemsAL = L.map snd
withKey k a = (k,a)

deleteByKey k = L.deleteBy keyEq (k,undefined)

-- Strictly evaluluate structure and keys but not elements.
force [] = []
force l@((k,_):rest) = k `seq` force rest `seq` l

seqMaybe Nothing b = b
seqMaybe (Just a) b = a `seq` b
	
al = AL . force

unboxInt (I# i) = i

instance Eq k => Map (AList k) where

	type Key (AList k) = k
	
	empty = al []
	
	singleton k a = al [(k,a)]
	
	pair k1 k2 = 
		if 	k1 == k2
		then	Nothing
		else	Just $ \ a1 a2 -> al [(k1,a1),(k2,a2)]
		 
	status (AL []) = None
	status (AL [(k,a)]) = One k a
	status _ = Many
	
	addSize (AL as) = (+#) (unboxInt (L.length as))
	
	lookup k (AL as) = L.lookup k as
	
	alter f k (AL as) = 
		let 	ma = L.lookup k as
		in	case (ma, f ma) of
				(Nothing, Nothing) 	-> al as
				(Nothing, Just a) 	-> al $ (k,a):as
				(Just _, Nothing) 	-> al $ deleteByKey k as
				(Just _, Just a)	-> al $ ((k,a):) $ deleteByKey k as 
				
	vennMaybe f (AL as) (AL bs) =
		let	leftDiff = 	[ (k,a) | (k,a) <- as , isNothing (L.lookup k bs) ]
			rightDiff = 	[ (k,b) | (k,b) <- bs , isNothing (L.lookup k as) ]
			inter =	
				let 	ks = L.intersect (keysOf as) (keysOf bs)
					assoc k = do
						a <- L.lookup k as
						b <- L.lookup k bs
						value <- f a b
						return (k,value)
				in	catMaybes (L.map assoc ks)
		in	(al leftDiff,al inter,al rightDiff)
				
	disjointUnion (AL as) (AL bs) = al (as ++ bs)
		
	isSubsetOf (AL as) (AL bs) = L.all (flip L.elem (keysOf bs)) (keysOf as)
	 
	isSubmapOf f (AL as) (AL bs) = L.all (\ (k,a) -> (Just True) == (fmap (f a) $ L.lookup k bs)) as
	
	map f (AL as) = al $ L.map (\(k,a) -> (k,f a)) as
	map' f (AL as) = al $ L.map (\(k,a) -> let a' = f a in a' `seq` (k,a')) as
	
	mapMaybe f (AL as) = al $ catMaybes $ L.map (\(k,a) -> fmap (withKey k) $ f a ) as
	
	mapWithKey f (AL as) = al $ L.map (\ (k,a) -> (k,f k a)) as
	mapWithKey' f (AL as) = al $ L.map (\(k,a) -> let a' = f k a in a' `seq` (k,a')) as
	
	filter f (AL as) = al $ L.filter (f . snd) as
	
	foldElems f b (AL as) = L.foldr f b $ elemsAL as
	foldKeys f b (AL as) = L.foldr f b $ keysOf as
	foldAssocs f b (AL as) = L.foldr (\(k,a) acc -> f k a acc) b as 
	
	foldElems' f b (AL as) = L.foldl' (flip f) b $ elemsAL as
	foldKeys' f b (AL as) = L.foldl' (flip f) b $ keysOf as
	foldAssocs' f b (AL as) = L.foldl' (\acc (k,a) -> f k a acc) b as 
	
	foldElemsUInt f i (AL as) = fold i as
		where	fold i' []     = i'
			fold i' ((_,a):as') = fold (f a i') as'
	
	valid (AL as) = 
		if 	keysOf as == (L.nub $ keysOf as)
		then 	Nothing
		else	Just "Duplicate keys"
		
-- Sorted assoc list with no duplicate keys
-- The map argument is used to determine the ordering used
newtype SList (mp :: * -> *) a = SL [(Key mp,a)] 

sl :: OrderedMap mp => [(Key mp,a)] -> SList mp a
sl kas = 
    let mp :: SList mp a -> (mp a)
        mp = undefined
        result = SL $ force $ L.sortBy (\ (k1,_) (k2,_) -> compareKey (mp result) k1 k2) kas
    in  result

instance (OrderedMap mp) => Map (SList mp) where

	type Key (SList mp) = Key mp

	empty = SL []
	
	singleton k a = SL [(k,a)]
	
	pair k1 k2 = 
		if 	k1 == k2
		then	Nothing
		else	Just $ \ a1 a2 -> sl [(k1,a1),(k2,a2)]
		 
	status (SL []) = None
	status (SL [(k,a)]) = One k a
	status _ = Many
	
	addSize (SL as) = (+#) (unboxInt (L.length as))
	
	lookup k (SL as) = L.lookup k as
	
	alter f k (SL as) = 
		let 	ma = L.lookup k as
		in	case (ma, f ma) of
				(Nothing, Nothing) 	-> SL as
				(Nothing, Just a) 	-> sl $ (k,a):as
				(Just _, Nothing) 	-> SL $ deleteByKey k as
				(Just _, Just a)	-> sl $ ((k,a):) $ deleteByKey k as 
	
	vennMaybe f (SL as) (SL bs) =
		let	leftDiff = 	[ (k,a) | (k,a) <- as , isNothing (L.lookup k bs) ]
			rightDiff = 	[ (k,b) | (k,b) <- bs , isNothing (L.lookup k as) ]
			inter =	
				let 	ks = L.intersect (keysOf as) (keysOf bs)
					assoc k = do
						a <- L.lookup k as
						b <- L.lookup k bs
						value <- f a b
						return (k,value)
				in	catMaybes (L.map assoc ks)
		in	(sl leftDiff,sl inter,sl rightDiff)
				
	disjointUnion (SL as) (SL bs) = sl (as ++ bs)
		
	isSubsetOf (SL as) (SL bs) = L.all (flip L.elem (keysOf bs)) (keysOf as)  
	
	isSubmapOf f (SL as) (SL bs) = L.all (\ (k,a) -> (Just True) == (fmap (f a) $ L.lookup k bs)) as  
	
	map f (SL as) = sl $ L.map (\(k,a) -> (k,f a)) as
	map' f (SL as) = sl $ L.map (\(k,a) -> let a' = f a in a' `seq` (k,a')) as
	
	mapMaybe f (SL as) = sl $ catMaybes $ L.map (\(k,a) -> fmap (withKey k) $ f a ) as
	
	mapWithKey f (SL as) = sl $ L.map (\ (k,a) -> (k,f k a)) as
	mapWithKey' f (SL as) = sl $ L.map (\(k,a) -> let a' = f k a in a' `seq` (k,a')) as
	
	filter f (SL as) = SL $ L.filter (f . snd) as
	
	foldElems f b (SL as) = L.foldr f b $ elemsAL as
	foldKeys f b (SL as) = L.foldr f b $ keysOf as
	foldAssocs f b (SL as) = L.foldr (\(k,a) acc -> f k a acc) b as 
	
	foldElems' f b (SL as) = L.foldl' (flip f) b $ reverse $ elemsAL as
	foldKeys' f b (SL as) = L.foldl' (flip f) b $ reverse $ keysOf as
	foldAssocs' f b (SL as) = L.foldl' (\acc (k,a) -> f k a acc) b $ reverse as 
	
	foldElemsUInt f i (SL as) = fold i as
		where	fold i' []     = i'
			fold i' ((_,a):as') = fold (f a i') as'
	
	valid sl@(SL as) 
		| keysOf as /= (L.nub $ keysOf as)				= Just "Duplicate keys"
		| keysOf as /= (L.sortBy (compareKey (mp sl)) $ keysOf as)	= Just "Unsorted"
		| otherwise							= Nothing
		where mp = undefined :: SList mp a -> mp a
		
instance (OrderedMap mp) => OrderedMap (SList mp) where
	
	compareKey sl = compareKey (mp sl)
	   where mp :: SList mp a -> (mp a)
	         mp = undefined 
	
	foldAssocsAsc f b (SL as) = L.foldr (uncurry f) b as
	foldAssocsDesc f b (SL as) = L.foldr (uncurry f) b $ reverse as
	
	foldAssocsAsc' f b (SL as) = L.foldl' (flip $ uncurry f) b $ reverse as
	foldAssocsDesc' f b (SL as) = L.foldl' (flip $ uncurry f) b as
   	
-- A map type to tell SList to behave use standard Orderings
data ImaginaryOrdMap k a
instance Eq k => Map (ImaginaryOrdMap k) where
	type Key (ImaginaryOrdMap k) = k
instance (Eq k, Ord k) => OrderedMap (ImaginaryOrdMap k) where
	compareKey _ = compare

type OList k = SList (ImaginaryOrdMap k) k
	
	
-- instance (Eq k, Ord k) => OrdMap (SList k) k