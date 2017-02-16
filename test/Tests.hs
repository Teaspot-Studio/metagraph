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
        testCase "Created nodes are queriable" $ isJust (indexNode (NodeId 0) testGraph1) @?= True
      , testCase "Created nodes are queriable" $ isJust (indexNode (NodeId 1) testGraph1) @?= True
      , testCase "Non existed nodes are not queriable" $ isJust (indexNode (NodeId 2) testGraph1) @?= False
    ]
  , testGroup "indexEdge" [
        testCase "Created edges are queriable" $ isJust (indexEdge (EdgeId 0) testGraph1) @?= True
      , testCase "Created edges are queriable" $ isJust (indexEdge (EdgeId 1) testGraph1) @?= True
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
