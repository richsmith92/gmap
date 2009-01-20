{-# OPTIONS_GHC -fglasgow-exts -Wall -fno-warn-missing-signatures #-}

module Data.GMap.EitherMap
(
 EitherMap
) where

import Data.GMap

import Data.GMap.ChoiceMap
import Data.GMap.InjectKeys

--------------------------------------------------------------------------------------------
--                     Map Type for Either                 --
--------------------------------------------------------------------------------------------

data InjectEither l r

instance Injection (InjectEither l r) (Choice2 l r) where
	type K1 (InjectEither l r) = Either l r

	inject _ (Left l)  = C1of2 l
	inject _ (Right r) = C2of2 r
	outject _ (C1of2 l) = Left l
	outject _ (C2of2 r) = Right r

type EitherMap mapL mapR = InjectKeys (InjectEither (Key mapL) (Key mapR)) (Choice2 (Key mapL) (Key mapR)) (Choice2Map mapL mapR)