{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module TRES
  ( TRES(..)
  , parseTRES
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Word (Word16, Word32, Word64)

data TRES = TRES
  { tresCPU :: !Word32
  , tresMem :: !Word64
  , tresGPU :: !Word16
  , tresNode :: !Word16
  } deriving (Show)

instance Semigroup TRES where
  TRES n1 c1 m1 g1 <> TRES n2 c2 m2 g2 =
    TRES (n1 + n2) (c1 + c2) (m1 + m2) (g1 + g2)

instance Monoid TRES where
  mempty = TRES 0 0 0 0

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
