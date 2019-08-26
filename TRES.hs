{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module TRES
  ( TRES(..)
  , parseTRES
  , Alloc(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Fixed (Centi)
import           Data.Word (Word16, Word32, Word64)

data TRES = TRES
  { tresCPU :: !Word32
  , tresMem :: !Word64
  , tresGPU :: !Word16
  , tresNode :: !Word16
  } deriving (Show)

instance Num TRES where
  TRES c1 m1 g1 n1 + TRES c2 m2 g2 n2 =
    TRES (c1 + c2) (m1 + m2) (g1 + g2) (n1 + n2)
  TRES c1 m1 g1 n1 - TRES c2 m2 g2 n2 =
    TRES (c1 - c2) (m1 - m2) (g1 - g2) (n1 - n2)
  TRES c1 m1 g1 n1 * TRES c2 m2 g2 n2 =
    TRES (c1 * c2) (m1 * m2) (g1 * g2) (n1 * n2)
  abs (TRES c m g n) = 
    TRES (abs c) (abs m) (abs g) (abs n)
  negate (TRES c m g n) = 
    TRES (negate c) (negate m) (negate g) (negate n)
  signum (TRES c m g n) = 
    TRES (signum c) (signum m) (signum g) (signum n)
  fromInteger n =
    TRES 0 0 0 (fromInteger n)

instance Semigroup TRES where
  (<>) = (+)

instance Monoid TRES where
  mempty = 0

parseTRES :: BS.ByteString -> TRES
parseTRES s = foldMap br $ BSC.split ',' s where
  br (BSC.break ('=' ==) -> (k, BSC.uncons -> Just ('=', v))) = pt k v
  br _ = mempty
  pt "node" (pn -> Just n) = mempty{ tresNode = fromInteger n }
  pt "cpu" (pn -> Just n) = mempty{ tresCPU = fromInteger n }
  pt "mem" (pn -> Just n) = mempty{ tresMem = fromInteger n }
  pt "gres/gpu" (pn -> Just n) = mempty{ tresGPU = fromInteger n }
  pt _ _ = mempty
  pn (BSC.readInteger -> Just (n, flip lookup units -> Just u)) = Just (u * n)
  pn _ = Nothing
  units = zip ["","K","M","G","T","P"] $ iterate (1024*) 1

data Alloc = Alloc
  { allocTRES :: !TRES
  , allocJob :: !Word
  , allocLoad :: !Centi
  , allocMem :: !Word64 -- ^actually used memory
  } deriving (Show)

instance Num Alloc where
  Alloc c1 m1 g1 n1 + Alloc c2 m2 g2 n2 =
    Alloc (c1 + c2) (m1 + m2) (g1 + g2) (n1 + n2)
  Alloc c1 m1 g1 n1 - Alloc c2 m2 g2 n2 =
    Alloc (c1 - c2) (m1 - m2) (g1 - g2) (n1 - n2)
  Alloc c1 m1 g1 n1 * Alloc c2 m2 g2 n2 =
    Alloc (c1 * c2) (m1 * m2) (g1 * g2) (n1 * n2)
  abs (Alloc c m g n) = 
    Alloc (abs c) (abs m) (abs g) (abs n)
  negate (Alloc c m g n) = 
    Alloc (negate c) (negate m) (negate g) (negate n)
  signum (Alloc c m g n) = 
    Alloc (signum c) (signum m) (signum g) (signum n)
  fromInteger n =
    Alloc 0 0 0 (fromInteger n)

instance Semigroup Alloc where
  (<>) = (+)

instance Monoid Alloc where
  mempty = 0

