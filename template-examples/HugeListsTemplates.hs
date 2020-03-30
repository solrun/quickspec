-- A stress test using lots and lots of list functions.
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
import Data.List
import Control.Monad
import QuickSpec.Internal.Utils(usort)
main = qqSpec [
  con "length" (length :: [A] -> Int),
  con "sort" (sort :: [Int] -> [Int]),
  con "scanr" (scanr :: (A -> B -> B) -> B -> [A] -> [B]),
  con "succ" (succ :: Int -> Int),
  con ">>=" ((>>=) :: [A] -> (A -> [B]) -> [B]),
  con "snd" (snd :: (A, B) -> B),
  con "reverse" (reverse :: [A] -> [A]),
  con "0" (0 :: Int),
  con "," ((,) :: A -> B -> (A, B)),
  con ">=>" ((>=>) :: (A -> [B]) -> (B -> [C]) -> A -> [C]),
  con ":" ((:) :: A -> [A] -> [A]),
  con "break" (break :: (A -> Bool) -> [A] -> ([A], [A])),
  con "filter" (filter :: (A -> Bool) -> [A] -> [A]),
  con "scanl" (scanl :: (B -> A -> B) -> B -> [A] -> [B]),
  con "zipWith" (zipWith :: (A -> B -> C) -> [A] -> [B] -> [C]),
  con "concat" (concat :: [[A]] -> [A]),
  con "zip" (zip :: [A] -> [B] -> [(A, B)]),
  con "usort" (usort :: [Int] -> [Int]),
  con "sum" (sum :: [Int] -> Int),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "map" (map :: (A -> A) -> [A] -> [A]),
  con "foldl" (foldl :: (A -> A -> A) -> A -> [A] -> A),
  con "takeWhile" (takeWhile :: (A -> Bool) -> [A] -> [A]),
  con "foldr" (foldr :: (A -> A -> A) -> A -> [A] -> A),
  con "drop" (drop :: Int -> [A] -> [A]),
  con "dropWhile" (dropWhile :: (A -> Bool) -> [A] -> [A]),
  con "span" (span :: (A -> Bool) -> [A] -> ([A], [A])),
  con "unzip" (unzip :: [(A, B)] -> ([A], [B])),
  con "+" ((+) :: Int -> Int -> Int),
  con "[]" ([] :: [A]),
  con "partition" (partition :: (A -> Bool) -> [A] -> ([A], [A])),
  con "fst" (fst :: (A, B) -> A),
  con "take" (take :: Int -> [A] -> [A])
  ,template "2-2-distributive" "?F(F,?G(X,Y)) = ?G(?F(F,X),?F(F,Y))"
  ,template "1-2-distributive" "?F(F,?G(X)) = ?G(?F(F,X))"
  ,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
  ,template "commutative" "?F(X,Y) = ?F(Y,X)"
  ,template "nest-commutative" "?F(?G(X,Y)) = ?F(?G(Y,X))"
  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ,template "cancel" "?F(?G(X)) = ?F(X)"
  ,template "cancel-2" "?F(?G(F,X)) = ?F(X)"
  ,template "comp-id" "?F(?G(X))=X"
  ,template "op-id-elem" "?F(X,?G) = X"
  ,template "op-zero-elem" "?F(X,?G) = ?G"
  ,template "fix-point" "?F(?X) = ?X"
   -- example of a template producing larger properties
  ,template "2o-2-2-distributive" "?F(?H(Z),?G(X,Y)) = ?G(?F(?H(Z),X),?F(?H(Z),Y))"
  ,template "hm" "?F(?G(X),?G(Y))=?F(?G(Y),?G(X))"
  ]
