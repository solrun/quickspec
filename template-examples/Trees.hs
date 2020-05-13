{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeOperators #-}
import QuickSpec
import Test.QuickCheck
import GHC.Generics
import qualified Data.List as L
import Test.QuickCheck.Poly(OrdA(..))
import Data.Function(on)
import Data.Ord(comparing)

data BST k v = Leaf | Branch (BST k v ) k v (BST k v )
  deriving (Eq, Show , Generic, Typeable)
-- the operations under test


instance (Ord k, Ord v) => Ord (BST k v) where
  compare = comparing toList

nil :: BST k v
nil = Leaf

insert :: Ord k => (k, v) -> BST k v -> BST k v
insert (k, v) Leaf = Branch Leaf k v Leaf
insert (k, v) (Branch l k' v' r) | k == k' = Branch l k v r
                                 | k < k' = Branch (insert (k,v) l) k' v' r
                                 | otherwise = Branch l k' v' (insert (k,v) r)

delete :: Ord k => k -> BST k v -> BST k v
delete _ Leaf = Leaf
delete k (Branch l k' v' r) | k == k' = union l r
                            | otherwise = Branch (delete k l) k' v' (delete k r)

union :: Ord k => BST k v -> BST k v -> BST k v
union Leaf t = t
union t Leaf = t
union t1@(Branch l1 k1 v1 r1) t2@(Branch l2 k2 v2 r2)
  | k1 == k2 = Branch (union l1 l2) k1 v1 (union r1 r2)
  | k1 < k2  = union (Branch l1 k1 v1 (insert (k2, v2) $ union r1 r2)) l2
  | otherwise = union t2 t1

find :: Ord k => k -> BST k v -> Maybe v
find _ Leaf = Nothing
find k (Branch l k' v' r) | k == k' = Just v'
                          | otherwise =
                            case (find k l) of
                              Just lv -> Just lv
                              Nothing -> case find k r of
                                Just rv -> Just rv
                                Nothing -> Nothing

instance (Ord k , Arbitrary k , Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    kvs  <- arbitrary
    return $ foldr insert nil (kvs :: [(k, v)])
  shrink = genericShrink

-- auxiliary operations
toList :: BST k v -> [(k,v)]
toList Leaf = []
toList (Branch l k v r) = toList l ++ [(k,v)] ++ toList r

keys :: BST k v -> [k]
keys = (map fst) . toList

deleteKey :: Ord k => k -> [(k,v)] -> [(k,v)]
deleteKey k = filter ((/= k) . fst)

unionLists :: Ord k => [(k,v)] -> [(k,v)] -> [(k,v)]
unionLists = L.unionBy ((==) `on` fst)

insertList :: (Ord k, Ord v) => (k,v) -> [(k,v)] -> [(k,v)]
insertList (k,v) l = L.insert (k,v) $ deleteKey k l

prop_thingy :: BST Int Int -> BST Int Int -> Bool
prop_thingy t t' = toList (union t t') == L.sort (unionLists (toList t) (toList t'))

main = quickCheck prop_thingy
--  qqSpec  [
--  monoType (Proxy :: Proxy OrdA),
--  inst (Sub Dict :: () :- Arbitrary (BST Int Int)),
--  monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (BST OrdA OrdA)),
--  --monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (BST OrdA Int)),
--  con "nil"    (nil :: BST OrdA OrdA),
--  con "insert" (insert :: (OrdA, OrdA) -> BST OrdA OrdA -> BST OrdA OrdA),
--  con "delete" (delete :: OrdA -> BST OrdA OrdA -> BST OrdA OrdA),
--  con "union"  (union :: BST OrdA OrdA -> BST OrdA OrdA -> BST OrdA OrdA),
--  con "find"   (find :: OrdA -> BST OrdA OrdA -> Maybe OrdA),
--  background [
--      con "toList" (toList :: BST OrdA OrdA -> [(OrdA,OrdA)]),
--      con "keys"   (keys :: BST OrdA OrdA -> [OrdA]),
--      "sort"       `con` (L.sort     :: [OrdA] -> [OrdA]),
--      con "insertList" (insertList :: (OrdA,OrdA) -> [(OrdA,OrdA)] -> [(OrdA,OrdA)]),
--      con "[]" ([] :: [(OrdA,OrdA)]),
--      con "deleteKeyList" (deleteKey :: OrdA -> [(OrdA,OrdA)] -> [(OrdA,OrdA)]),
--      con "unionList" (unionLists :: [(OrdA,OrdA)] -> [(OrdA,OrdA)] -> [(OrdA,OrdA)]),
--      con "findList" (L.lookup :: OrdA -> [(OrdA,OrdA)] -> Maybe OrdA)
--             ]
--  ,template "id" "?F(X)=X"
--  ,template "fix-point" "?F(?X) = ?X"
--  ,template "cancel" "?F(?G(X)) = ?F(X)"
--  ,template "left-id-elem" "?F(?G,X) = X"
--  ,template "right-id-elem" "?F(X,?G) = X"
--  ,template "commutative" "?F(X,Y) = ?F(Y,X)"
--  ,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
--  ,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
--  ,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
--  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
--  ,template "model-based" "?F(toList(X)) = toList(?G(X))"
--  ,template "model-based-2" "?F(Y,toList(X)) = toList(?G(Y,X))"
--  ,template "model-based-2-1" "?F(Y,toList(X)) = ?G(Y,X)"
--  ,template "model-based-0" "toList(?X) = ?Y"
--  ,template "model-based-distributive" "?F(toList(X),toList(Y)) = toList(?H(X,Y))"
--  ]
