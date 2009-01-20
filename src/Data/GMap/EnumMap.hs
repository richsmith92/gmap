{-# OPTIONS_GHC -fglasgow-exts -Wall -fno-warn-missing-signatures #-}

module Data.GMap.EnumMap
(-- * EnumMap type
 EnumMap
) where

import Data.GMap()

import Data.GMap.IntMap
import Data.GMap.InjectKeys

--------------------------------------------------------------------------------------------
--                     Map Type for 'Enum'erable keys                   --
--------------------------------------------------------------------------------------------

data InjectEnum k

instance Enum k => Injection (InjectEnum k) Int where
	type K1 (InjectEnum k) = k

	inject _ = fromEnum
	outject _ = toEnum

type EnumMap k = InjectKeys (InjectEnum k) Int IntMap