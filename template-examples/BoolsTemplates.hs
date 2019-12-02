{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
main = qqSpec [
  predicate "not" not,
  con "True" True,
  con "False" False,
  con "||" (||),
  con "&&" (&&)
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
  ]
