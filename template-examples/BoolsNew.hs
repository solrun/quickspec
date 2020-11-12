{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
--import Data.Time
mySig = [
  con "not" not,
  con "True" True,
  con "False" False,
  con "||" (||),
  con "&&" (&&)
  ,template "comp-id" "?F(?G(X))=X"
  ,template "fix-point" "?F(?X) = ?X"
  ,template "cancel" "?F(?G(X)) = ?F(X)"
  ,template "op-id-elem" "?F(X,?G) = X"
  ,template "commutative" "?F(X,Y) = ?F(Y,X)"
  ,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
  ,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
  ,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ]
main = do
  --start <- getCurrentTime
  roughSpec mySig
  --qqTime <- getCurrentTime
  quickSpec mySig
  --qsTime <- getCurrentTime
  --print (diffUTCTime qqTime start)
  --print (diffUTCTime qsTime qqTime)
