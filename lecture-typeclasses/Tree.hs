data Tree a = Node (Tree a) a (Tree a)
            | Empty
            deriving (Show)

instance Functor Tree where
    fmap f (Node l n r) = Node (fmap f l) (f n) (fmap f r)
    fmap _ (Empty)      = Empty
