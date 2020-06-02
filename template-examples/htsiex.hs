{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
data BST k v = Leaf | Branch (BST k v ) k v (BST k v )
  deriving (Eq, Show , Generic)
-- the operations under test
find :: Ord k => k -> BST k v -> Maybe v
find = undefined
nil :: BST k v
nil = undefined
insert :: Ord k => k -> v -> BST k v -> BST k v
insert = undefined
delete :: Ord k => k -> BST k v -> BST k v
delete = undefined
union :: Ord k  => BST k v -> BST k v -> BST k v
union = undefined
-- auxiliary operations
toList :: BST k v -> [(k , v )]
toList = undefined
keys :: BST k v -> [k ]
keys = undefined

main = return ()
