module Data.Metagraph.Builder(
  -- * Types
    MetaGraphM
  -- * Basic API
  , buildMetaGraph
  , newMetaGraph
  , newNode
  , newEdge
  , addNode
  , addEdge
  -- * Helpers
  , lookupGraph
  , lookupEdge
  , lookupNode
  ) where

import Data.Metagraph.Strict
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M
import Control.Monad.State.Strict
import Data.Metagraph.Internal.Types
import Data.Maybe

-- | Build environment for 'MetaGraphM'
data BuildEnv edge node = BuildEnv {
-- | Next id for edge
  _buildNextEdge  :: !Int
-- | Next id for node
, _buildNextNode  :: !Int
-- | Next id for graph
, _buildNextGraph :: !Int
-- | Cache of all edges
, _buildEdges     :: !(IntMap (MetaEdge edge node))
-- | Cache of all nodes
, _buildNodes     :: !(IntMap (MetaNode edge node))
-- | Cache of all graphs
, _buildGraphs    :: !(IntMap (MetaGraph edge node))
}

-- | Construct new building environment
newBuildEnv :: BuildEnv edge node
newBuildEnv = BuildEnv 0 0 0 mempty mempty mempty

-- | Increment edge id counter and return old value
genNextEdgeId :: MetaGraphId -> (EdgeId -> MetaEdge edge node) -> BuildEnv edge node -> (EdgeId, BuildEnv edge node)
genNextEdgeId !gi !e !be = be' `seq` i `seq` (EdgeId i, be')
  where
    i = _buildNextEdge be
    edge = e $! EdgeId i
    be' = be {
        _buildNextEdge = _buildNextEdge be + 1
      , _buildEdges    = M.insert i edge $! _buildEdges be
      , _buildGraphs   = M.adjust (insertEdge edge) (unMetaGraphId gi) $! _buildGraphs be
      }

-- | Increment node id counter and return old value
genNextNodeId :: MetaGraphId -> (NodeId -> MetaNode edge node) -> BuildEnv edge node -> (NodeId, BuildEnv edge node)
genNextNodeId !gi !n !be = be' `seq` i `seq` (NodeId i, be')
  where
    i = _buildNextNode be
    node = n $! NodeId i
    be' = be {
        _buildNextNode = _buildNextNode be + 1
      , _buildNodes    = M.insert i node $! _buildNodes be
      , _buildGraphs   = M.adjust (insertNode node) (unMetaGraphId gi) $! _buildGraphs be
      }

-- | Increment node id counter and return old value
genNextGraphId :: (MetaGraphId -> MetaGraph edge node) -> BuildEnv edge node -> (MetaGraphId, BuildEnv edge node)
genNextGraphId !g !be = be' `seq` i `seq` (MetaGraphId i, be')
  where
    i = _buildNextGraph be
    be' = be {
        _buildNextGraph = _buildNextGraph be + 1
      , _buildGraphs    = M.insert i (g $ MetaGraphId i) $! _buildGraphs be
      }

-- | Find edge in build environment
envLookupEdge :: EdgeId -> BuildEnv edge node -> Maybe (MetaEdge edge node)
envLookupEdge !i !be = M.lookup (unEdgeId i) $! _buildEdges be

-- | Find node in build environment
envLookupNode :: NodeId -> BuildEnv edge node -> Maybe (MetaNode edge node)
envLookupNode !i !be = M.lookup (unNodeId i) $! _buildNodes be

-- | Find edge in build environment
envLookupGraph :: MetaGraphId -> BuildEnv edge node -> Maybe (MetaGraph edge node)
envLookupGraph !i !be = M.lookup (unMetaGraphId i) $! _buildGraphs be

-- | Adjusting value of edge by id in build environment
envModifyEdges :: EdgeId -> (MetaEdge edge node -> MetaEdge edge node) -> BuildEnv edge node -> BuildEnv edge node
envModifyEdges !i !f !be = be { _buildEdges = M.adjust f (unEdgeId i) $! _buildEdges be }

-- | Adjusting value of node by id in build environment
envModifyNodes :: NodeId -> (MetaNode edge node -> MetaNode edge node) -> BuildEnv edge node -> BuildEnv edge node
envModifyNodes !i !f !be = be { _buildNodes = M.adjust f (unNodeId i) $! _buildNodes be }

-- | Adjusting value of metagraph by id in build environment
envModifyGraphs :: MetaGraphId -> (MetaGraph edge node -> MetaGraph edge node) -> BuildEnv edge node -> BuildEnv edge node
envModifyGraphs !i !f !be = be { _buildGraphs = M.adjust f (unMetaGraphId i) $! _buildGraphs be }

-- | Remove edge from build environment
envDeleteEdge :: EdgeId -> BuildEnv edge node -> BuildEnv edge node
envDeleteEdge !i !be = be { _buildEdges = M.delete (unEdgeId i) $! _buildEdges be }

-- | Remove node from build environment
envDeleteNode :: NodeId -> BuildEnv edge node -> BuildEnv edge node
envDeleteNode !i !be = be { _buildNodes = M.delete (unNodeId i) $! _buildNodes be }

-- | Remove graph from build environment
envDeleteGraph :: MetaGraphId -> BuildEnv edge node -> BuildEnv edge node
envDeleteGraph !i !be = be { _buildGraphs = M.delete (unMetaGraphId i) $! _buildGraphs be }

-- | A special monad where creation of meta graph is efficient
newtype MetaGraphM edge node a = MetaGraphM { unMetaGraphM :: State (BuildEnv edge node) a }
  deriving (Functor, Applicative, Monad)

-- | Execute monadic action that builds metagraph efficiently. Return value in the monadic
-- builder is resulted metagraph that is extracted from build environment.
buildMetaGraph :: MetaGraphM edge node MetaGraphId -> MetaGraph edge node
buildMetaGraph ma = fromMaybe
  (error "buildMetaGraph: resulting graph is not in environment!")
  (M.lookup (unMetaGraphId i) $ _buildGraphs env)
  where
    (i, env) = runState (unMetaGraphM ma) newBuildEnv

-- | Creation of new metagraph in monadic action
newMetaGraph :: MetaGraphM edge node MetaGraphId
newMetaGraph = MetaGraphM . state $ genNextGraphId empty

-- | Creation of new node in monadic action
newNode :: MetaGraphId -- ^ Metagraph the node belongs to
  -> Maybe MetaGraphId -- ^ Subgraph
  -> node -- ^ Payload
  -> MetaGraphM edge node NodeId
newNode gi msubi payload = do
  mgr <- join <$> traverse lookupGraph msubi
  MetaGraphM . state $ genNextNodeId gi $ \i -> MetaNode {
      _nodeId      = i
    , _nodePayload = payload
    , _nodeGraph   = mgr
    }

-- | Add given node to metagraph
addNode :: MetaGraphId -- ^ Target metagraph
  -> NodeId -- ^ Existing node
  -> MetaGraphM edge node ()
addNode gi ni = do
  mn <- lookupNode ni
  case mn of
    Nothing -> fail "addNode: cannot find node with such id"
    Just n -> MetaGraphM . modify' $ envModifyGraphs gi $ \g -> g {
        _metagraphNodes = M.insert (unNodeId ni) n $! _metagraphNodes g
      }

-- | Add given edge to metagraph
addEdge :: MetaGraphId -- ^ Target metagraph
  -> EdgeId -- ^ Existing edge
  -> MetaGraphM edge node ()
addEdge gi ei = do
  mn <- lookupEdge ei
  case mn of
    Nothing -> fail "addEdge: cannot find edge with such id"
    Just e -> MetaGraphM . modify' $ envModifyGraphs gi $ \g -> g {
        _metagraphEdges = M.insert (unEdgeId ei) e $! _metagraphEdges g
      }

-- | Create new edge in build environment
newEdge :: MetaGraphId -- ^ Metagraph the edge belongs to
  -> Directed -- ^ Is the edge directed
  -> NodeId -- ^ Start node
  -> NodeId -- ^ End node
  -> Maybe MetaGraphId -- ^ Subgraph
  -> edge -- ^ Paload
  -> MetaGraphM edge node EdgeId
newEdge gi dir starti endi msubi payload = do
  mgr <- join <$> traverse lookupGraph msubi
  from <- maybe (fail $ "Cannot find from node " ++ show starti) return =<< lookupNode starti
  to   <- maybe (fail $ "Cannot find to node " ++ show starti)   return =<< lookupNode endi
  MetaGraphM . state $ genNextEdgeId gi $ \i -> MetaEdge {
      _edgeId       = i
    , _edgeDirected = dir
    , _edgeFrom     = from
    , _edgeTo       = to
    , _edgePayload  = payload
    , _edgeGraph    = mgr
    }

-- | Find metagraph in build environment
lookupGraph :: MetaGraphId -> MetaGraphM edge node (Maybe (MetaGraph edge node))
lookupGraph i = MetaGraphM $ envLookupGraph i <$> get

-- | Find metagraph in build environment
lookupEdge :: EdgeId -> MetaGraphM edge node (Maybe (MetaEdge edge node))
lookupEdge i = MetaGraphM $ envLookupEdge i <$> get

-- | Find metagraph in build environment
lookupNode :: NodeId -> MetaGraphM edge node (Maybe (MetaNode edge node))
lookupNode i = MetaGraphM $ envLookupNode i <$> get
