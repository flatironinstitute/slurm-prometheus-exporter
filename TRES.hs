{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module TRES
  ( TRES(..)
  , parseTRES
  , GRES(..)
  , parseGPUs
  , Alloc(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Fixed (Centi)
import qualified Data.Map.Strict as Map
import           Data.String (IsString)
import           Data.Word (Word16, Word32, Word64)
import           System.Posix.Types (EpochTime)

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
parseTRES = foldMap br . BSC.split ',' where
  br (BSC.break ('=' ==) -> (k, BSC.uncons -> Just ('=', v))) = pt k v
  br _ = mempty
  pt "node" (pn -> Just n) = mempty{ tresNode = fromInteger n }
  pt "cpu" (pn -> Just n) = mempty{ tresCPU = fromInteger n }
  pt "mem" (pn -> Just n) = mempty{ tresMem = fromInteger n }
  pt "gres/gpu" (pn -> Just n) = mempty{ tresGPU = fromInteger n }
  pt _ _ = mempty
  pn (BSC.readInteger -> Just (n, flip lookup units -> Just u)) = Just $ u * n
  pn (reads . BSC.unpack -> [(n, flip lookup units -> Just u)]) = Just $ round $ u * (n :: Double)
  pn _ = Nothing
  units :: (IsString u, Num n) => [(u, n)]
  units = zip ["","K","M","G","T","P"] $ iterate (1024*) 1

newtype GRES = GRES { gresMap :: Map.Map BS.ByteString Word16 }
  deriving (Show)

instance Num GRES where
  GRES g1 + GRES g2 = GRES (Map.unionWith (+) g1 g2)
  GRES g1 - GRES g2 = GRES (Map.unionWith (+) g1 (negate <$> g2))
  GRES g1 * GRES g2 = GRES (Map.intersectionWith (*) g1 g2)
  abs (GRES g) = GRES (abs <$> g)
  negate (GRES g) = GRES (negate <$> g)
  signum (GRES g) = GRES (signum <$> g)
  fromInteger _ = GRES mempty

instance Semigroup GRES where
  (<>) = (+)

instance Monoid GRES where
  mempty = 0

parseGPUs :: BS.ByteString -> GRES
parseGPUs = GRES . foldMap br . BSC.split ',' where
  br (BSC.break (':' ==) -> ("gpu", BSC.uncons ->
    Just (':', BSC.break (':' ==) -> (t, BSC.uncons -> 
      Just (':', BSC.readInteger -> Just (n, r))))))
    | ispar r = Map.singleton t (fromInteger n)
  br _ = mempty
  ispar r = case BSC.uncons r of
    Nothing -> True
    Just ('(', _) -> True
    _ -> False

data Alloc = Alloc
  { allocTRES :: !TRES
  , allocJob :: !Word
  , allocLoad :: !Centi
  , allocMem :: !Word64 -- ^actually used memory
  , allocTime :: !EpochTime
  , allocGPUs :: GRES
  } deriving (Show)

instance Num Alloc where
  Alloc c1 m1 l1 n1 t1 g1 + Alloc c2 m2 l2 n2 t2 g2 =
    Alloc (c1 + c2) (m1 + m2) (l1 + l2) (n1 + n2) (t1 + t2) (g1 + g2)
  Alloc c1 m1 l1 n1 t1 g1 - Alloc c2 m2 l2 n2 t2 g2 =
    Alloc (c1 - c2) (m1 - m2) (l1 - l2) (n1 - n2) (t1 - t2) (g1 - g2)
  Alloc c1 m1 l1 n1 t1 g1 * Alloc c2 m2 l2 n2 t2 g2 =
    Alloc (c1 * c2) (m1 * m2) (l1 * l2) (n1 * n2) (t1 * t2) (g1 * g2)
  abs (Alloc c m l n t g) =
    Alloc (abs c) (abs m) (abs l) (abs n) (abs t) (abs g)
  negate (Alloc c m l n t g) =
    Alloc (negate c) (negate m) (negate l) (negate n) (negate t) (negate g)
  signum (Alloc c m l n t g) =
    Alloc (signum c) (signum m) (signum l) (signum n) (signum t) (signum g)
  fromInteger n =
    Alloc 0 (fromInteger n) 0 0 0 0

instance Semigroup Alloc where
  (<>) = (+)

instance Monoid Alloc where
  mempty = 0

