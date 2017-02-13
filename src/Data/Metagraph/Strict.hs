{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Metagraph.Strict(
    MetaGraph
  , MetaEdge
  , MetaNode
  -- * Index primary info
  , metagraphEdges
  , metagraphNodes
  , edgeId
  , edgeDirected
  , edgePayload
  , edgeGraph
  , nodeId
  , nodePayload
  , nodeGraph
  ) where

import Data.Metagraph.Internal.Types
import Data.IntMap
import Data.Bifunctor

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
edgeDirected :: MetaEdge edge node -> Bool
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
  fmap !f !e = graph `seq` e {
      _edgeGraph = graph
    }
    where
      graph = fmap f <$> _edgeGraph e
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
  first !f !e = payload `seq` graph `seq` e {
      _edgePayload = payload
    , _edgeGraph   = graph
    }
    where
      payload = f $ _edgePayload e
      graph   = first f <$> _edgeGraph e
  second !f !e = graph `seq` e {
      _edgeGraph = graph
    }
    where
      graph = second f <$> _edgeGraph e
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
