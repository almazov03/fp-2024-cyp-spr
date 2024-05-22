module TreeTest where

import Test.HUnit
import Tree

tree :: Tree Int
tree = Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)

treeTests :: Test
treeTests = TestList [
    TestCase $ assertEqual "inOrder tree" [1, 2, 3, 4] (inOrder tree),
    TestCase $ assertEqual "preOrder tree" [3, 1, 2, 4] (preOrder tree),
    TestCase $ assertEqual "postOrder tree" [2, 1, 4, 3] (postOrder tree)
  ]
