{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}

import Prelude hiding (null)
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Poly(OrdA(..))
import Data.Ord
import qualified Data.List as L

data Heap a = Nil | Branch Int a (Heap a) (Heap a) deriving (Typeable, Show)

instance Ord a => Eq (Heap a) where
  h1 == h2 = toList h1 == toList h2

instance Ord a => Ord (Heap a) where
  compare = comparing toList

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = fmap fromList arbitrary

toList :: Ord a => Heap a -> [a]
toList h | null h = []
         | otherwise = findMin h:toList (deleteMin h)

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert Nil

null :: Heap a -> Bool
null Nil = True
null _ = False

findMin :: Heap a -> a
findMin (Branch _ x _ _) = x

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge h (branch x Nil Nil)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Branch _ _ l r) = merge l r
deleteMin Nil = Nil

branch :: Ord a => a -> Heap a -> Heap a -> Heap a
branch x l r | npl l <= npl r = Branch (npl l + 1) x l r
             | otherwise = Branch (npl r + 1) x r l

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge h1@(Branch _ x1 l1 r1) h2@(Branch _ x2 l2 r2)
 | x1 <= x2 = branch x1 (merge l1 h2) r1
 | otherwise = merge h2 h1

npl :: Heap a -> Int
npl Nil = 0
npl (Branch n _ _ _) = n

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] xs = xs
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y = x:mergeLists xs (y:ys)
  | otherwise = y:mergeLists (x:xs) ys

prop_delete :: Heap Int -> Bool
prop_delete h = toList(deleteMin h) == tail (toList h)

main = quickCheck prop_delete
--  qqSpec [
--  prelude `without` ["*"],
--  monoType (Proxy :: Proxy OrdA),
--  monoTypeWithVars ["h", "h1", "h2"] (Proxy :: Proxy (Heap OrdA)),
--
--  "nil"        `con` (Nil        :: Heap OrdA),
--  "insert"     `con` (insert     :: OrdA -> Heap OrdA -> Heap OrdA),
--  "findMin"    `con` (findMin    :: Heap OrdA -> OrdA),
--  "deleteMin"  `con` (deleteMin  :: Heap OrdA -> Heap OrdA),
--  "merge"      `con` (merge      :: Heap OrdA -> Heap OrdA -> Heap OrdA),
--  "null"       `con` (null       :: Heap OrdA -> Bool),
--  --"fromList"   `con` (fromList   :: [OrdA] -> Heap OrdA),
--
--  -- A few more list functions that are helpful for getting
--  -- laws about toList/fromList.
--  -- We use "background" to mark the functions as background theory,
--  -- so that we only get laws that involve one of the heap functions.
--  background [
--  con "True" True,
--  con "False" False,
--  "head" `con` (head :: [A] -> A),
--  "tail" `con` (tail :: [A] -> [A]),
--  "toList"     `con` (toList     :: Heap OrdA -> [OrdA]),
--  "sort"       `con` (L.sort     :: [OrdA] -> [OrdA]),
--  "insertList" `con` (L.insert   :: OrdA -> [OrdA] -> [OrdA]),
--  "nullList"   `con` (L.null     :: [OrdA] -> Bool),
--  "deleteList" `con` (L.delete   :: OrdA -> [OrdA] -> [OrdA]),
--  "mergeLists" `con` (mergeLists :: [OrdA] -> [OrdA] -> [OrdA])]
--  --,template "id" "?F(X)=X"
--  --,template "fix-point" "?F(?X) = ?X"
--  --,template "cancel" "?F(?G(X)) = ?F(X)"
--  --,template "left-id-elem" "?F(?G,X) = X"
--  --,template "right-id-elem" "?F(X,?G) = X"
--  --,template "commutative" "?F(X,Y) = ?F(Y,X)"
--  --,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
--  --,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
--  --,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
--  --,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
--  ,template "model-based" "?F(toList(X)) = toList(?G(X))"
--  --,template "model-based-2" "?F(Y,toList(X)) = toList(?G(Y,X))"
--  ,template "model-based-2-1" "?F(Y,toList(X)) = ?G(Y,X)"
--  ,template "model-based-0" "toList(?X) = ?Y"
--  ,template "model-based-distributive" "toList(?H(X,Y)) = ?F(toList(X),toList(Y))"
--  ,template "model-based-pred" "?P(X) = ?Q(toList(X))"
--  ]
