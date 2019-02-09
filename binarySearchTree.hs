data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldMap _ EmptyTree = mempty
  foldMap f (Node x left right) = foldMap f left `mappend`
                                  f x            `mappend`
                                  foldMap f right

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
_ `treeElem` EmptyTree = False
x `treeElem` (Node a left right)
  | x == a = True
  | x < a = x `treeElem` left
  | x > a = x `treeElem` right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

treeToList :: Tree a -> [a]
treeToList = foldMap (\x -> [x])
