{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Metagraph.Strict(
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
  -- * Modifying metagraphs
  , insertNode
  , insertEdge
  ) where

import Data.Metagraph.Internal.Types
import Data.IntMap    (IntMap)
import Data.Bifunctor
import qualified Data.IntMap.Strict as M

-- | Return metagraph id
metagraphId :: MetaGraph edge node -> MetaGraphId
metagraphId !m = _metagraphId m

-- | Return top-level edges of metagraph
metagraphEdges :: MetaGraph edge node -> IntMap (MetaEdge edge node)
metagraphEdges !m = _metagraphEdges m

-- | Return top-level nodes of metagraph
metagraphNodes :: MetaGraph edge node -> IntMap (MetaNode edge node)
metagraphNodes !m = _metagraphNodes m

-- | Get unique ID of edge
edgeId :: MetaEdge edge node -> EdgeId
edgeId !v = _edgeId v

-- | Get edge directed flag
edgeDirected :: MetaEdge edge node -> Directed
edgeDirected !v = _edgeDirected v

-- | Get edge payload
edgePayload :: MetaEdge edge node -> edge
edgePayload !v = _edgePayload v

-- | Get edge subgraph
edgeGraph :: MetaEdge edge node -> Maybe (MetaGraph edge node)
edgeGraph !v = _edgeGraph v

-- | Get unique ID of node
nodeId :: MetaNode edge node -> NodeId
nodeId !v = _nodeId v

-- | Get node payload
nodePayload :: MetaNode edge node -> node
nodePayload !v = _nodePayload v

-- | Get node subgraph
nodeGraph :: MetaNode edge node -> Maybe (MetaGraph edge node)
nodeGraph !v = _nodeGraph v

-- | Make meta graph without edges and nodes
empty :: MetaGraphId -> MetaGraph edge node
empty i = i `seq` nodes `seq` edges `seq` MetaGraph {
    _metagraphId    = i
  , _metagraphEdges = edges
  , _metagraphNodes = nodes
  }
  where
    edges = mempty
    nodes = mempty

-- | Insert new node into metagraph. Note: uniquness of id is not checked.
insertNode :: MetaNode edge node -> MetaGraph edge node -> MetaGraph edge node
insertNode node gr = nodes `seq` gr {
    _metagraphNodes = nodes
  }
  where
    nodes = M.insert (unNodeId . _nodeId $ node) node $ _metagraphNodes gr

-- | Insert new edge into metagraph. Note: uniquness of id is not checked.
insertEdge :: MetaEdge edge node -> MetaGraph edge node -> MetaGraph edge node
insertEdge edge gr = edges `seq` gr {
    _metagraphEdges = edges
  }
  where
    edges = M.insert (unEdgeId . _edgeId $ edge) edge $ _metagraphEdges gr

instance Functor (MetaGraph edge) where
  fmap !f !m = nodes `seq` edges `seq` m {
      _metagraphEdges = edges
    , _metagraphNodes = nodes
    }
    where
      edges = fmap f <$> _metagraphEdges m
      nodes = fmap f <$> _metagraphNodes m
  {-# INLINE fmap #-}

instance Functor (MetaEdge edge) where
  fmap !f !e = from `seq` to `seq` graph `seq` e {
      _edgeGraph = graph
    , _edgeFrom  = from
    , _edgeTo    = to
    }
    where
      graph = fmap f <$> _edgeGraph e
      from  = f <$> _edgeFrom e
      to    = f <$> _edgeTo e
  {-# INLINE fmap #-}

instance Functor (MetaNode edge) where
  fmap !f !n = payload `seq` graph `seq` n {
      _nodePayload = payload
    , _nodeGraph   = graph
    }
    where
      graph   = fmap f <$> _nodeGraph n
      payload = f $ _nodePayload n
  {-# INLINE fmap #-}

instance Bifunctor MetaGraph where
  first !f !m = nodes `seq` edges `seq` m {
      _metagraphEdges = edges
    , _metagraphNodes = nodes
    }
    where
      edges = first f <$> _metagraphEdges m
      nodes = first f <$> _metagraphNodes m

  second !f !m = nodes `seq` edges `seq` m {
      _metagraphEdges = edges
    , _metagraphNodes = nodes
    }
    where
      edges = second f <$> _metagraphEdges m
      nodes = second f <$> _metagraphNodes m
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifunctor MetaEdge where
  first !f !e = from `seq` to `seq` payload `seq` graph `seq` e {
      _edgePayload = payload
    , _edgeGraph   = graph
    , _edgeFrom    = from
    , _edgeTo      = to
    }
    where
      payload = f $ _edgePayload e
      graph   = first f <$> _edgeGraph e
      from    = first f $ _edgeFrom e
      to      = first f $ _edgeTo e

  second !f !e =  from `seq` to `seq` graph `seq` e {
      _edgeGraph = graph
    , _edgeFrom  = from
    , _edgeTo    = to
    }
    where
      graph = second f <$> _edgeGraph e
      from  = second f $ _edgeFrom e
      to    = second f $ _edgeTo e
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifunctor MetaNode where
  first !f !n = graph `seq` n {
      _nodeGraph   = graph
    }
    where
      graph   = first f <$> _nodeGraph n

  second !f !n = payload `seq` graph `seq` n {
      _nodePayload = payload
    , _nodeGraph   = graph
    }
    where
      graph   = second f <$> _nodeGraph n
      payload = f $ _nodePayload n
  {-# INLINE first #-}
  {-# INLINE second #-}
