import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Maybe
import Data.Metagraph
import Data.Metagraph.Internal.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests = testGroup "Unit tests" [
    testGroup "indexNode" [
        testCase "Created nodes are queriable1" $ isJust (indexNode (NodeId 0) testGraph1) @?= True
      , testCase "Created nodes are queriable2" $ isJust (indexNode (NodeId 1) testGraph1) @?= True
      , testCase "Created nodes are queriable3" $ isJust (indexNode (NodeId 1) testGraph2) @?= True
      , testCase "Non existed nodes are not queriable" $ isJust (indexNode (NodeId 2) testGraph1) @?= False
    ]
  , testGroup "indexEdge" [
        testCase "Created edges are queriable1" $ isJust (indexEdge (EdgeId 0) testGraph1) @?= True
      , testCase "Created edges are queriable2" $ isJust (indexEdge (EdgeId 1) testGraph1) @?= True
      , testCase "Created edges are queriable3" $ isJust (indexEdge (EdgeId 1) testGraph2) @?= True
      , testCase "Non existed edges are not queriable" $ isJust (indexEdge (EdgeId 2) testGraph1) @?= False
    ]
  ]

-- | Simple planar graph
testGraph1 :: MetaGraph String String
testGraph1 = buildMetaGraph $ do
  g <- newMetaGraph
  a <- newNode g Nothing "A"
  b <- newNode g Nothing "B"
  _ <- newEdge g Directed a b Nothing "1"
  _ <- newEdge g Directed b a Nothing "2"
  return g

-- | Non-planar metagraph
testGraph2 :: MetaGraph String String
testGraph2 = buildMetaGraph $ do
  g  <- newMetaGraph
  gA <- newMetaGraph
  a1 <- newNode gA Nothing "A1"
  a2 <- newNode gA Nothing "A2"
  a3 <- newNode gA Nothing "A3"
  _ <- newEdge gA Directed a1 a2 Nothing "a12"
  _ <- newEdge gA Directed a2 a3 Nothing "a23"
  _ <- newEdge gA Directed a3 a1 Nothing "a31"
  a  <- newNode g (Just gA) "A"
  b  <- newNode g Nothing "B"
  _ <- newEdge g Directed b a1 Nothing "ba1"
  return g
