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

buildTree :: Hashable a => [a] -> Tree a
buildTree t = 
  let ls = map leaf t:[buildTreeHelper l | l <- ls] 
  in head $ head $ filter (null . tail) ls
  where
    buildTreeHelper [] = []
    buildTreeHelper [x] = [twig x]
    buildTreeHelper (x1:x2:xs) = node x1 x2:buildTreeHelper xs

drawTree :: Show a => Tree a -> String
drawTree t = drawTreeWithIndent t 0 where
  drawTreeWithIndent (Leaf h v) i = replicate (2*i) ' ' ++ showHash h ++ " " ++ show v ++ "\n"
  drawTreeWithIndent (Node1 h t1) i = replicate (2*i) ' ' ++ showHash h ++ " +\n" ++ drawTreeWithIndent t1 (i+1)
  drawTreeWithIndent (Node2 h t1 t2) i = replicate (2*i) ' ' ++ showHash h ++ " -\n" ++ drawTreeWithIndent t1 (i+1) ++ drawTreeWithIndent t2 (i+1)

-- Part B

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x (Leaf h y) = [[] | hash x == h]
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
  showsPrec d (MerkleProof x mp) = showParen (d > app_prec) $
   showString "MerkleProof " . showsPrec (app_prec + 1) x . showString " " . showString (showMerklePath mp) where
     app_prec = 10

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof x mp) = h == foldr (\a b -> case a of
 Left h1 -> hash (b, h1)
 Right h2 -> hash (h2, b)) (hash x) mp
