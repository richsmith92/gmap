{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -fallow-incoherent-instances -XRank2Types -fno-monomorphism-restriction #-}

module Test.GMap.Utils where

import Test.QuickCheck

import Data.GMap
import Data.GMap.ChoiceMap
import qualified Data.List as L
import Control.Monad(liftM)

import Data.GMap.AssocList

import System.Random(newStdGen)

gen n g = do
	stdg <- newStdGen
	return $ generate n stdg g

-- eg use: (Just `on` (+))       is        (\a b -> Just (a + b))
on f g a b = f (g a b)

-- ### QuickCheck instances ###

instance Show (a->b) where
	show _ = "<function>"

instance (OrderedMap map, Arbitrary (Key map), Arbitrary a) => Arbitrary (map a) where
	arbitrary = liftM fromAssocs (arbitrary :: Gen [(Key map,a)])
	coarbitrary mp = coarbitrary (assocs mp)

instance (OrderedMap map, Show (Key map), Show a) => Show (map a) where
	show map = "fromAssocs " ++ (show $ assocs map)

instance Arbitrary Char where
    arbitrary = sized $ \n -> choose (minBound , maxBound `min` (toEnum n))
    coarbitrary c = variant (fromEnum c)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e) => Arbitrary (a,b,c,d,e) where
    arbitrary = do
    	(a,b,c,(d,e)) <- arbitrary
    	return (a,b,c,d,e)
    coarbitrary (a,b,c,d,e) = coarbitrary (a,b,c,(d,e))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Choice2 a b) where
   arbitrary = oneof [C1of2 `fmap` arbitrary, C2of2 `fmap` arbitrary]
   coarbitrary choice = case choice of
   	C1of2 a -> coarbitrary a
   	C2of2 b -> coarbitrary b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Choice3 a b c) where
   arbitrary = oneof [C1of3 `fmap` arbitrary, C2of3 `fmap` arbitrary, C3of3 `fmap` arbitrary]
   coarbitrary choice = case choice of
   	C1of3 a -> coarbitrary a
   	C2of3 b -> coarbitrary b
   	C3of3 c -> coarbitrary c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Choice4 a b c d) where
   arbitrary = oneof [C1of4 `fmap` arbitrary, C2of4 `fmap` arbitrary, C3of4 `fmap` arbitrary, C4of4 `fmap` arbitrary]
   coarbitrary choice = case choice of
   	C1of4 a -> coarbitrary a
   	C2of4 b -> coarbitrary b
   	C3of4 c -> coarbitrary c
   	C4of4 d -> coarbitrary d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e) => Arbitrary (Choice5 a b c d e) where
   arbitrary = oneof [C1of5 `fmap` arbitrary, C2of5 `fmap` arbitrary, C3of5 `fmap` arbitrary, C4of5 `fmap` arbitrary, C5of5 `fmap` arbitrary]
   coarbitrary choice = case choice of
   	C1of5 a -> coarbitrary a
   	C2of5 b -> coarbitrary b
   	C3of5 c -> coarbitrary c
   	C4of5 d -> coarbitrary d
   	C5of5 e -> coarbitrary e

-- These functions are used to pass types around as undefined arguments.
like = const :: a -> a -> a
likeElem = const :: OrderedMap map => a -> map a -> a
likeMaybeElem = const :: OrderedMap map => Maybe a -> map a -> Maybe a

-- Test type (allows specifying type of map used in tests)
data Test m1 m2 where
	-- A simple test - pass in a map and get out something testable
	SimpleTest :: Testable b => (m1 -> b) -> Test m1 m2
	-- A simple test that requires two maps. Used for set ops etc
	SimpleTest2 :: Testable b => ((m1,m1) -> b) -> Test m1 m2
	-- CompareTest the behaviour of two different maps
	CompareTest :: (Arbitrary a, Show a, Eq b) =>
		(m1 -> a -> b) -> (m2 -> a -> b) -> Test m1 m2
	CompareTest2 :: (Arbitrary a, Show a, Eq b) =>
		((m1,m1) -> a -> b) -> ((m2,m2) -> a -> b) -> Test m1 m2

compareTest :: (OrderedMap mp1, OrderedMap mp2, Arbitrary a, Show a, Eq b, Key mp1 ~ Key mp2) => (forall mp. (OrderedMap mp) => (mp e) -> a -> b) -> Test (mp1 e) (mp2 e)
compareTest f = CompareTest f f
compareTest2 :: (OrderedMap mp1, OrderedMap mp2, Arbitrary a, Show a, Eq b, Key mp1 ~ Key mp2) => (forall mp. (OrderedMap mp) => ((mp e),(mp e)) -> a -> b) -> Test (mp1 e) (mp2 e)
compareTest2 f = CompareTest2 f f

-- Unsurprisingly Tests are Testable
instance (OrderedMap mp1, OrderedMap mp2, Show (mp1 a), Show (mp2 a), Arbitrary (Key mp1), Arbitrary a, Show (Key mp1), Show a, Key mp1 ~ Key mp2) => Testable (Test (mp1 a) (mp2 a)) where
	property (SimpleTest f) = property f
	property (SimpleTest2 f) = property f
	property (CompareTest f1 f2) = property (\ kas a -> f1 (fromAssocs kas) a == f2 (fromAssocs kas) a)
	property (CompareTest2 f1 f2) = property (\ kas1 kas2 a -> f1 (fromAssocs kas1, fromAssocs kas2) a == f2 (fromAssocs kas1, fromAssocs kas2) a)

-- Used to generate lists of tests by parsing the source file
-- Its unfortunate that its necessary, better introspection would make life easier
testList file prefix code = do
	source <- readFile file
	let props = L.filter (\l -> (L.isPrefixOf prefix l) && (not $ L.isPrefixOf (prefix ++ " ::") l)) $
		    L.map head $ L.filter (not.null) $ L.map words $ lines source
	let printProp prop = do
		putStr "("
		putStr (code ++ prop)
		putStr ",\""
		putStr prop
		putStr "\")"
	putStr "["
	printProp $ head props
	mapM_ (\prop -> do
		putStr ","
		printProp prop) $ tail props
	putStrLn "]"

config n = Config
	{ configMaxTest = n
	, configMaxFail = 1000
	, configSize    = (+ 3) . (`div` 2)
	, configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
	}

-- A list of named tests
type Tests m1 m2 = [(Test m1 m2, String)]

runTests :: (Testable (Test m1 m2)) => Tests m1 m2 -> Int -> IO ()
runTests tests n =
	mapM_ ( \ (prop,name) -> do
		putStr name
		putStr " : "
		check (config n) prop ) tests

-- Narrows the type of runTests using the type of the first argument
runAListTest :: (OrderedMap mp, Testable (Test (mp a) (AList k a))) => (mp a) -> Tests (mp a) (AList k a) -> Int -> IO ()
runSListTest :: (OrderedMap mp, Testable (Test (mp a) (SList mp a))) => (mp a) -> Tests (mp a) (SList mp a) -> Int -> IO ()
runAListTest _ = runTests
runSListTest _ = runTests