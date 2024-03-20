{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
--import Data.Time
mySig = [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  arith (Proxy :: Proxy Int)
  ,template "fix-point/id" "?F(?X) = ?X"
  ,template "left-id-elem" "?F(?Y,X) = X"
  ,template "right-id-elem" "?F(X,?Y) = X"
  ,template "cancel" "?F(?G(X)) = ?F(X)"
  ,template "commutative" "?F(X,Y) = ?F(Y,X)"
  ,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
  ,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
  ,template "analogy-distributive" "?G(?H(X,Y)) = ?F(?G(X),?G(Y))"
  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ]
main = do
--  start <- getCurrentTime
  quickSpec mySig
  roughSpec mySig
--  qqTime <- getCurrentTime
--  quickSpec mySig
--  qsTime <- getCurrentTime
--  print (diffUTCTime qqTime start)
--  print (diffUTCTime qsTime qqTime)
