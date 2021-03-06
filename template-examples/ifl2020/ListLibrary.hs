-- A stress test using lots and lots of list functions.
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
import Data.List
import Control.Monad
import QuickSpec.Internal.Utils(usort)
import Data.Time
mySig = [
  con "length" (length :: [A] -> Int),
  con "sort" (sort :: [Int] -> [Int]),
  con "scanr" (scanr :: (A -> B -> B) -> B -> [A] -> [B]),
  con ">>=" ((>>=) :: [A] -> (A -> [B]) -> [B]),
  con "reverse" (reverse :: [A] -> [A]),
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
  con "[]" ([] :: [A]),
  con "partition" (partition :: (A -> Bool) -> [A] -> ([A], [A])),
  con "take" (take :: Int -> [A] -> [A]),
  background [
  con "," ((,) :: A -> B -> (A, B)),
  con "fst" (fst :: (A, B) -> A),
  con "snd" (snd :: (A, B) -> B),
  con "+" ((+) :: Int -> Int -> Int),
  con "0" (0 :: Int),
  con "succ" (succ :: Int -> Int)
            ]
  --,template "id" "?F(X)=X"
  --,template "fixpoint" "?F(?X) = ?X"
  --,template "cancel" "?F(?G(X)) = ?F(X)"
  --,template "left-id-elem" "?F(?Y,X) = X"
  --,template "right-id-elem" "?F(X,?Y) = X"
  --,template "commutative" "?F(X,Y) = ?F(Y,X)"
  --,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
  --,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
  --,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
  --,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ]

bgSig = [
  con "," ((,) :: A -> B -> (A, B)),
  con "fst" (fst :: (A, B) -> A),
  con "snd" (snd :: (A, B) -> B),
  con "+" ((+) :: Int -> Int -> Int),
  con "0" (0 :: Int),
  con "succ" (succ :: Int -> Int)
            ]
main = do
  start <- getCurrentTime
  --quickSpec bgSig
  roughSpecDefault mySig
  rs1Time <- getCurrentTime
--  quickSpec mySig
--  qsTime <- getCurrentTime
  roughSpecQSDefault mySig
  rsqsTime <- getCurrentTime
  roughSpecWithQuickSpec 3 $ mySig ++ [defaultTemplates]
  rsqs3Time <- getCurrentTime
  print (diffUTCTime rs1Time start)
  print (diffUTCTime rsqsTime rs1Time)
  print (diffUTCTime rsqs3Time rsqsTime)
