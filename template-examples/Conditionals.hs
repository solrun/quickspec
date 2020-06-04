{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import Data.List
import QuickSpec
import Data.Time

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] xs = xs
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y = x:mergeLists xs (y:ys)
  | otherwise = y:mergeLists (x:xs) ys

sorted :: Ord a => [a] -> Bool
sorted xs = sort xs == xs

mySig = [
--  con "reverse" (reverse :: [A] -> [A]),
--  con "++" ((++) :: [A] -> [A] -> [A]),
--  con "[]" ([] :: [A]),
--  con "map" (map :: (A -> B) -> [A] -> [B]),
--  con "length" (length :: [A] -> Int),
--  con "concat" (concat :: [[A]] -> [A]),
--  con "sort" (sort :: [Int] -> [Int]),
--  con "usort" (nub . sort :: [Int] -> [Int]),
  con "mergeLists" (mergeLists :: [Int] -> [Int] -> [Int]),
 -- con "insert" (insert :: Int -> [Int] -> [Int]),
  con "False" False,
  con "True" True,
  predicate "sorted" (sorted :: [Int] -> Bool)
  --arith (Proxy :: Proxy Int)
  ,template "not sorted" "?F(X) = False"
  ,template "sorted" "?F(X) = True"
--  ,template "fix-point/id" "?F(?X) = ?X"
--  ,template "left-id-elem" "?F(?Y,X) = X"
--  ,template "right-id-elem" "?f(X,?Y) = X"
--  ,template "cancel" "?F(?G(X)) = ?F(X)"
  ,template "commutative" "?F(X,Y) = ?F(Y,X)"
--  ,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
--  ,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
--  ,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
--  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ]
main = do
--  start <- getCurrentTime
  qqSpec mySig
--  qqTime <- getCurrentTime
--  quickSpec mySig
--  qsTime <- getCurrentTime
--  print (diffUTCTime qqTime start)
--  print (diffUTCTime qsTime qqTime)
