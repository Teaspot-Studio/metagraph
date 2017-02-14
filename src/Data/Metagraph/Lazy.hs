{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Metagraph.Lazy(
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
  -- * Creation of metagraph
  , empty
  ) where

import           Data.Bifunctor
import           Data.IntMap                   (IntMap)
import           Data.Metagraph.Internal.Types

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
edgeDirected :: MetaEdge edge node -> Bool
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
empty :: MetaGraph edge node
empty = MetaGraph {
    _metagraphEdges = mempty
  , _metagraphNodes = mempty
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
    }
  second f e = e {
      _edgeGraph   = second f <$> _edgeGraph e
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
