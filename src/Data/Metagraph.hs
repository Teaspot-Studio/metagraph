{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}
module Data.Metagraph(
    Direction(..)
  , Metagraph
  , MetaNode
  , MetaEdge
  ) where

import GHC.Generics
import Control.Lens

import qualified Data.HashMap.Strict as HM

-- | Phantom kind to indicate direction property of metagraph
data Direction =
    Directed -- ^ Phantom type to indicate directed graph
  | Undirected -- ^ Phantom type to indicate undirected graph
  deriving (Generic, Eq, Show, Enum, Bounded)

-- | Node of metagraph, can be metagraph or another node.
--
--    [@e@] Data payload for edges;
--
--    [@n@] Data payload for node.
data Metagraph e n = Metagraph {
  metagraphNodes :: [MetaNode e n] -- ^ Nodes that are not attached to any edge
, metagraphDirectedEdges :: [MetaEdge Directed e n] -- ^ Directed edges with nodes
, metagraphUndirectedEdges :: [MetaEdge Undirected e n] -- ^ Undirected edges with nodes
}

-- | Node of metagraph, can be metagraph or another node.
--
--    [@e@] Data payload for edges;
--
--    [@n@] Data payload for node.
data MetaNode e n = MetaNode {
  metaNodeData :: n -- ^ Data payload
, metaNodeGraph :: Maybe (Metagraph e n) -- ^ Subgraph of the node.
  -- ^ Subgraph of nodes must consists of undirected edges only.
}


-- | Edge of metagraph, can contain subgraph.
--
-- In @MetaEdge d e v@:
--
--    [@d@] Direction property of the edge;
--
--    [@e@] Data payload for edges;
--
--    [@n@] Data payload for nodes;
data MetaEdge (d :: Direction) e n = MetaEdge {
  metaEdgeData :: e -- ^ Data payload
, metaEdgeStart :: MetaNode e n -- ^ Start of the edge
, metaEdgeEnd :: MetaNode e n -- ^ End of the edge
, metaEdgeGraph :: Maybe (Metagraph e n) -- ^ Subgraph of the edge.
}
