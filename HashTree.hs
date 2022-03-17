module HashTree where
import Hashable32


data Tree a = Leaf Hash a | Node1 Hash (Tree a) | Node2 Hash (Tree a) (Tree a) deriving Show

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Node1 h _) = h
treeHash (Node2 h _ _) = h

leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig x = Node1 (hash (treeHash x, treeHash x)) x

node :: Hashable a => Tree a -> Tree a -> Tree a
node x y = Node2 (hash (treeHash x, treeHash y)) x y

buildTreeHelper :: Hashable a => [Tree a] -> [Tree a]
buildTreeHelper [] = []
buildTreeHelper [x] = [twig x]
buildTreeHelper (x1:x2:xs) = node x1 x2:buildTreeHelper xs

buildTreeHelper2 :: Hashable a => [Tree a] -> Tree a
buildTreeHelper2 [t] = t
buildTreeHelper2 ts = buildTreeHelper2 $ buildTreeHelper ts

buildTree :: Hashable a => [a] -> Tree a
buildTree t = buildTreeHelper2 $ map leaf t

drawTree :: Show a => Tree a -> String
drawTree t = drawTreeWithIndent t 0 where
  drawTreeWithIndent (Leaf h v) i = replicate i ' ' ++ showHash h ++ " " ++ show v ++ "\n"
  drawTreeWithIndent (Node1 h t1) i = replicate i ' ' ++ showHash h ++ " +\n" ++ drawTreeWithIndent t1 (i+1)
  drawTreeWithIndent (Node2 h t1 t2) i = replicate i ' ' ++ showHash h ++ " -\n" ++ drawTreeWithIndent t1 (i+1) ++ drawTreeWithIndent t2 (i+1)
