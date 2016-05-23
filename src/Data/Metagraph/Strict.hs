module Data.Metagraph.Strict(
    empty
  , singleton
  , addNode
  , addSubgraph
  , addMetaNode
  ) where

import Data.Metagraph.Impl
import qualified Data.HashMap.Strict as HM

-- | Metagraph without any edges or nodes
empty :: Metagraph e n
empty = Metagraph {
    metagraphNodes = HM.empty
  , metagraphDirectedEdges = HM.empty
  , metagraphUndirectedEdges = HM.empty
  , metagraphNextId = 0
  }

-- | Metagraph with single node
singleton :: n -> Metagraph e n
singleton n = addNode n empty

-- | Adds leaf node to metagraph
addNode :: n -> Metagraph e n -> Metagraph e n
addNode n = fst . addMetaNode n Nothing

-- | Adds meta node to metagraph
addSubgraph :: n -> Metagraph e n -> Metagraph e n -> Metagraph e n
addSubgraph n sub = fst . addMetaNode n (Just sub)

-- | Adds meta node to metagraph and returns id of added node
addMetaNode :: n -> Maybe (Metagraph e n) -> Metagraph e n -> (Metagraph e n, NodeId)
addMetaNode n sub m = metagraphNextId' `seq` metagraphNodes' `seq` mn `seq` (m', nid)
  where
  nid = NodeId $ metagraphNextId m
  mn = sub `seq` n `seq` nid `seq` MetaNode {
      metaNodeId = nid
    , metaNodeData = n
    , metaNodeGraph = sub
    }
  metagraphNodes' = HM.insert nid mn . metagraphNodes $ m
  metagraphNextId' = metagraphNextId m + 1
  m' = m {
      metagraphNodes = metagraphNodes'
    , metagraphNextId = metagraphNextId'
    }
