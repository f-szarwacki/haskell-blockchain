-- fs406372
module HashTree where
import Hashable32

-- Part A

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

-- >>> putStr $ drawTree $ buildTree "fubar"

-- Part B

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x (Leaf h y) = [[] | hash x == h] -- equality of x and y or just their hashes?
merklePaths x (Node1 h t) = map (Left (treeHash t):) (merklePaths x t)
merklePaths x (Node2 h t1 t2) = map (Left (treeHash t2):) (merklePaths x t1) ++ map (Right (treeHash t1):) (merklePaths x t2)

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t = case merklePaths x t of
  [] -> Nothing
  mp:_ -> Just (MerkleProof x mp)

showMerklePath :: MerklePath -> String
showMerklePath = concatMap showMerklePathElement where
  showMerklePathElement (Left h) = "<" ++ showHash h
  showMerklePathElement (Right h) = ">" ++ showHash h

instance Show a => Show (MerkleProof a) where
  show (MerkleProof x mp) = "MerkleProof " ++ show x ++ " " ++ showMerklePath mp -- TODO parenthesis!

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof x mp) = h == foldr (\a b -> case a of
 Left h1 -> hash (b, h1)
 Right h2 -> hash (h2, b)) (hash x) mp

-- >>> map showMerklePath $ merklePaths 'i' $ buildTree "bitcoin"

-- >>> buildProof 'i' $ buildTree "bitcoin"

-- >>> buildProof 'e' $ buildTree "bitcoin"