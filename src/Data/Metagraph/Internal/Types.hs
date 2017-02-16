module Data.Metagraph.Internal.Types(
    MetaGraphId(..)
  , MetaGraph(..)
  , Directed(..)
  , isDirected
  , isUndirected
  , EdgeId(..)
  , MetaEdge(..)
  , NodeId(..)
  , MetaNode(..)
  ) where

import           Data.IntMap
import           GHC.Generics
import Data.Bifunctor

-- | Id of edge in metagraph
newtype EdgeId = EdgeId { unEdgeId :: Int }
  deriving (Generic, Eq, Ord, Show)

-- | Id of node in metagraph
newtype NodeId = NodeId { unNodeId :: Int }
  deriving (Generic, Eq, Ord, Show)

-- | Id of metagraph
newtype MetaGraphId = MetaGraphId { unMetaGraphId :: Int }
  deriving (Generic, Eq, Ord, Show)

-- | Graph which edges and nodes can be a subgraphs.
--
-- [@edge@] Payload of edge
--
-- [@node@] Payload of node
data MetaGraph edge node = MetaGraph {
  _metagraphId    :: MetaGraphId
-- | Holds all top-level edges of graph
, _metagraphEdges :: IntMap (MetaEdge edge node)
-- | Holds all top-level nodes. This is nesseccary as there can be nodes that
-- are not connected with any other node.
, _metagraphNodes :: IntMap (MetaNode edge node)
} deriving (Generic)

-- | Direction marker for edge
data Directed = Directed | Undirected
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | Helper to check if the flag is directed
isDirected :: Directed -> Bool
isDirected Directed = True
isDirected _ = False

-- | Helper to check if the flag is undirected
isUndirected :: Directed -> Bool
isUndirected Undirected = True
isUndirected _ = False

-- | Edge of metagraph that can hold subgraph inside itself.
--
-- [@edge@] Payload of edge
--
-- [@node@] Payload of node
data MetaEdge edge node = MetaEdge {
-- | Unique id of edge
  _edgeId       :: EdgeId
-- | Holds direction feature of the edge
, _edgeDirected :: Directed
-- | Begin of the edge
, _edgeFrom     :: MetaNode edge node
-- | End of the edge
, _edgeTo       :: MetaNode edge node
-- | Payload of edge
, _edgePayload  :: edge
-- | Edge can hold subgraph
, _edgeGraph    :: Maybe (MetaGraph edge node)
} deriving (Generic)

-- | Node of metagraph that can hold subgraph inside itself.
data MetaNode edge node = MetaNode {
-- | Unique id of node
  _nodeId      :: NodeId
-- | Payload of node
, _nodePayload :: node
-- | Node can hold
, _nodeGraph   :: Maybe (MetaGraph edge node)
} deriving (Generic)
