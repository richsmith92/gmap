{-# OPTIONS_GHC -fglasgow-exts -Wall -fno-warn-missing-signatures #-}

module Data.GMap.MaybeMap
(-- * EnumMap type
 MaybeMap
) where

import Data.GMap

import Data.GMap.ChoiceMap
import Data.GMap.InjectKeys
import Data.GMap.UnitMap

--------------------------------------------------------------------------------------------
--                     Map Type for Maybe                 --
--------------------------------------------------------------------------------------------

data InjectMaybe k

instance Injection (InjectMaybe k) (Choice2 k ()) where
	type K1 (InjectMaybe k) = Maybe k

	inject _ (Just k)  = C1of2 k
	inject _ Nothing   = C2of2 ()
	outject _ (C1of2 k) = Just k
	outject _ (C2of2 _) = Nothing

type MaybeMap map = InjectKeys (InjectMaybe (Key map)) (Choice2 (Key map) ()) (Choice2Map map UnitMap)