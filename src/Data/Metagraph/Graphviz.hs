{-# LANGUAGE RecordWildCards, DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Data.Metagraph.Graphviz(
    dotMetagraph
  , dotMetagraphText
  ) where

import Data.Metagraph.Impl
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (Text, pack)
import qualified Data.HashMap.Strict as HM

-- | Simplified 'dotMetagraph' for text metagraph
dotMetagraphText :: GraphID -- ^ Name of graphviz graph
  -> Metagraph Text Text -- ^ Metagraph to convert
  -> DotGraph Text
dotMetagraphText gid = dotMetagraph gid
  (\MetaNode{..} -> (metaNodeData, Str metaNodeData, []))
  (\MetaEdge{..} -> (Str metaEdgeData, []))
  (\MetaEdge{..} -> (Str metaEdgeData, []))
  ""

-- | Convert metagraph into Graphviz representation
dotMetagraph :: forall e n n' . GraphID -- ^ Name of graphviz graph
  -> (MetaNode e n -> (n', GraphID, Attributes)) -- ^ How to render node
  -> (MetaEdge Directed e n -> (GraphID, Attributes)) -- ^ How to render directed edges
  -> (MetaEdge Undirected e n -> (GraphID, Attributes)) -- ^ How to render directed edges
  -> n' -- ^ Dummy node for edges between meta edges
  -> Metagraph e n -- ^ Metagraph to convert
  -> DotGraph n' -- ^ Resulting graphviz graph
dotMetagraph gid renderNode renderEdgeDir renderEdgeUndir dummy m = digraph gid $ dotMetagraph' m
  where
  dotMetagraph' :: Metagraph e n -> Dot n'
  dotMetagraph' Metagraph{..} = do
    node dummy [Style [SItem Invisible []]]
    mapM_ dotMetaNode $ snd <$> HM.toList metagraphNodes
    mapM_ dotMetaEdgeDir $ snd <$> HM.toList metagraphDirectedEdges
    mapM_ dotMetaEdgeUndir $ snd <$> HM.toList metagraphUndirectedEdges

  dotMetaNode :: MetaNode e n -> Dot n'
  dotMetaNode mn@MetaNode{..} = let (n', nid, attrs) = renderNode mn
    in case metaNodeGraph of
      Nothing -> node n' attrs
      Just subgraph -> cluster nid $ dotMetagraph' subgraph

  dotMetaEdge :: (MetaEdge d e n -> (GraphID, Attributes))
    -> Attributes -> MetaEdge d e n -> Dot n'
  dotMetaEdge renderEdge addAttrs me@MetaEdge{..} = do
    let (eid, attrs) = renderEdge me
        makeEndpoint mn endpos = let
          (e', eid, _) = renderNode mn
          in case metaNodeGraph mn of
            Nothing -> (e', [])
            Just _ -> (dummy, [endpos . pack . show $ eid])
        (f, fatrs) = makeEndpoint metaEdgeStart LTail
        (t, tatrs) = makeEndpoint metaEdgeEnd LHead
    case metaEdgeGraph of
      Nothing -> edge f t (addAttrs ++ attrs ++ fatrs ++ tatrs)
      Just subgraph -> do
        cluster eid $ dotMetagraph' subgraph
        edge f dummy (addAttrs ++ attrs ++ fatrs ++ [LHead . pack . show $ eid])
        edge dummy t (addAttrs ++ attrs ++ tatrs ++ [LTail . pack . show $ eid])

  dotMetaEdgeDir :: MetaEdge Directed e n -> Dot n'
  dotMetaEdgeDir = dotMetaEdge renderEdgeDir [Dir Forward]

  dotMetaEdgeUndir :: MetaEdge Undirected e n -> Dot n'
  dotMetaEdgeUndir = dotMetaEdge renderEdgeUndir [Dir NoDir]
