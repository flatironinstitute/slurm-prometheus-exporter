{-# LANGUAGE OverloadedStrings #-}

module Node
  ( nodeClass
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isAlpha, isDigit)
import           Data.Maybe (fromMaybe)

import Slurm

-- |Reduce a node name to some kind of classification. Specific to FI cluster config.
nodeClass :: NodeName -> NodeName 
nodeClass n
  | not (BS.null a) = a
  | not (BS.null d) = BS.take 1 d <> BSC.replicate (pred $ BS.length d) '0'
  | otherwise = mempty
  where
  a = BSC.takeWhile isAlpha s
  d = BSC.takeWhile isDigit s
  s = fromMaybe n $ BS.stripPrefix "worker" n

{-
data NodeClass = NodeClass
  { nodeClass :: NodeName
  , nodeState :: 

accountNodes :: [NodeInfo] -> [(Labels, TRES)]
accountNodes 
-}
