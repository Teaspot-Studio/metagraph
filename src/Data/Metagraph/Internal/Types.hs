module Data.Metagraph.Internal.Types(
    MetaGraph(..)
  , EdgeId
  , MetaEdge(..)
  , NodeId
  , MetaNode(..)
  ) where

import           Data.IntMap
import           GHC.Generics
import Data.Bifunctor

-- | Id of edge in metagraph
type EdgeId = Int

-- | Id of node in metagraph
type NodeId = Int

-- | Graph which edges and nodes can be a subgraphs.
--
-- [@edge@] Payload of edge
--
-- [@node@] Payload of node
data MetaGraph edge node = Metagraph {
-- | Holds all top-level edges of graph
  _metagraphEdges :: IntMap (MetaEdge edge node)
-- | Holds all top-level nodes. This is nesseccary as there can be nodes that
-- are not connected with any other node.
, _metagraphNodes :: IntMap (MetaNode edge node)
} deriving (Generic)

-- | Edge of metagraph that can hold subgraph inside itself.
--
-- [@edge@] Payload of edge
--
-- [@node@] Payload of node
data MetaEdge edge node = MetaEdge {
-- | Unique id of edge
  _edgeId       :: EdgeId
-- | Holds direction feature of the edge
, _edgeDirected :: Bool
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
