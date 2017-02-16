{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Metagraph.Lazy(
    MetaGraph
  , MetaEdge
  , MetaNode
  , NodeId
  , EdgeId
  , MetaGraphId
  , Directed(..)
  , isDirected
  , isUndirected
  -- * Index primary info
  , metagraphId
  , metagraphEdges
  , metagraphNodes
  , edgeId
  , edgeDirected
  , edgePayload
  , edgeGraph
  , nodeId
  , nodePayload
  , nodeGraph
  -- * Creation of metagraph
  , empty
  ) where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as M
import           Data.Maybe
import           Data.Metagraph.Internal.Types

-- | Return metagraph id
metagraphId :: MetaGraph edge node -> MetaGraphId
metagraphId = _metagraphId

-- | Return top-level edges of metagraph
metagraphEdges :: MetaGraph edge node -> IntMap (MetaEdge edge node)
metagraphEdges = _metagraphEdges

-- | Return top-level nodes of metagraph
metagraphNodes :: MetaGraph edge node -> IntMap (MetaNode edge node)
metagraphNodes = _metagraphNodes

-- | Get unique ID of edge
edgeId :: MetaEdge edge node -> EdgeId
edgeId = _edgeId

-- | Get edge directed flag
edgeDirected :: MetaEdge edge node -> Directed
edgeDirected = _edgeDirected

-- | Get edge payload
edgePayload :: MetaEdge edge node -> edge
edgePayload = _edgePayload

-- | Get edge subgraph
edgeGraph :: MetaEdge edge node -> Maybe (MetaGraph edge node)
edgeGraph = _edgeGraph

-- | Get unique ID of node
nodeId :: MetaNode edge node -> NodeId
nodeId = _nodeId

-- | Get node payload
nodePayload :: MetaNode edge node -> node
nodePayload = _nodePayload

-- | Get node subgraph
nodeGraph :: MetaNode edge node -> Maybe (MetaGraph edge node)
nodeGraph = _nodeGraph

-- | Make meta graph without edges and nodes
empty :: MetaGraphId -> MetaGraph edge node
empty i = MetaGraph {
    _metagraphId    = i
  , _metagraphEdges = mempty
  , _metagraphNodes = mempty
  }

-- | Insert new node into metagraph. Note: uniquness of id is not checked.
insertNode :: MetaNode edge node -> MetaGraph edge node -> MetaGraph edge node
insertNode node gr = gr {
    _metagraphNodes = M.insert (unNodeId . _nodeId $ node) node $ _metagraphNodes gr
  }

-- | Insert new edge into metagraph. Note: uniquness of id is not checked.
insertEdge :: MetaEdge edge node -> MetaGraph edge node -> MetaGraph edge node
insertEdge edge gr = gr {
    _metagraphEdges = M.insert (unEdgeId . _edgeId $ edge) edge $ _metagraphEdges gr
  }

instance Functor (MetaGraph edge) where
  fmap f m = m {
      _metagraphEdges = fmap f <$> _metagraphEdges m
    , _metagraphNodes = fmap f <$> _metagraphNodes m
    }
  {-# INLINE fmap #-}

instance Functor (MetaEdge edge) where
  fmap f e = e {
      _edgeGraph = fmap f <$> _edgeGraph e
    , _edgeFrom  = f <$> _edgeFrom e
    , _edgeTo    = f <$> _edgeTo e
    }
  {-# INLINE fmap #-}

instance Functor (MetaNode edge) where
  fmap f n = n {
      _nodePayload = f $ _nodePayload n
    , _nodeGraph   = fmap f <$> _nodeGraph n
    }
  {-# INLINE fmap #-}

instance Bifunctor MetaGraph where
  first f m = m {
      _metagraphEdges = first f <$> _metagraphEdges m
    , _metagraphNodes = first f <$> _metagraphNodes m
    }
  second f m = m {
      _metagraphEdges = second f <$> _metagraphEdges m
    , _metagraphNodes = second f <$> _metagraphNodes m
    }
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifunctor MetaEdge where
  first f e = e {
      _edgePayload = f $ _edgePayload e
    , _edgeGraph   = first f <$> _edgeGraph e
    , _edgeFrom    = first f $ _edgeFrom e
    , _edgeTo      = first f $ _edgeTo e
    }
  second f e = e {
      _edgeGraph   = second f <$> _edgeGraph e
    , _edgeFrom    = second f $ _edgeFrom e
    , _edgeTo      = second f $ _edgeTo e
    }
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifunctor MetaNode where
  first f n = n {
      _nodeGraph   = first f <$> _nodeGraph n
    }
  second f n = n {
      _nodePayload = f $ _nodePayload n
    , _nodeGraph   = second f <$> _nodeGraph n
    }
  {-# INLINE first #-}
  {-# INLINE second #-}
