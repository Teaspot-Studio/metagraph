{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}
module Data.Metagraph.Impl(
    Direction(..)
  , Metagraph(..)
  , NodeId(..)
  , MetaNode(..)
  , EdgeId(..)
  , MetaEdge(..)
  ) where

import GHC.Generics
import Control.Lens
import Data.Int
import Data.Hashable

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
  metagraphNodes :: HM.HashMap NodeId (MetaNode e n) -- ^ All nodes of the metagraph
, metagraphDirectedEdges :: HM.HashMap EdgeId (MetaEdge Directed e n) -- ^ All directed edges
, metagraphUndirectedEdges :: HM.HashMap EdgeId (MetaEdge Undirected e n) -- ^ All undirected edges
, metagraphNextId :: Int64 -- ^ Next not occupied id
}

-- | Unique node id based on int64
newtype NodeId = NodeId { unNodeId :: Int64 } deriving (Eq, Show, Generic)

instance Hashable NodeId

-- | Node of metagraph, can be metagraph or another node.
--
--    [@e@] Data payload for edges;
--
--    [@n@] Data payload for node.
data MetaNode e n = MetaNode {
  metaNodeId :: NodeId -- ^ Unique node id
, metaNodeData :: n -- ^ Data payload
, metaNodeGraph :: Maybe (Metagraph e n) -- ^ Subgraph of the node.
  -- ^ Subgraph of nodes must consists of undirected edges only.
}

-- | Unique edge id based on int64
newtype EdgeId = EdgeId { unEdgeId :: Int64 } deriving (Eq, Show, Generic)

instance Hashable EdgeId 

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
  metaEdgeId :: EdgeId -- ^ Unique edge id
, metaEdgeData :: e -- ^ Data payload
, metaEdgeStart :: MetaNode e n -- ^ Start of the edge
, metaEdgeEnd :: MetaNode e n -- ^ End of the edge
, metaEdgeGraph :: Maybe (Metagraph e n) -- ^ Subgraph of the edge.
}
