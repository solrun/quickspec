{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
main = qqSpec [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  arith (Proxy :: Proxy Int)
  ,template "id" "?F(X)=X"
  ,template "fix-point" "?F(?X) = ?X"
  ,template "cancel" "?F(?G(X)) = ?F(X)"
  ,template "op-id-elem" "?F(X,?G) = X"
  ,template "commutative" "?F(X,Y) = ?F(Y,X)"
  ,template "op-commute" "?F(?G(X)) = ?G(?F(X))"
  ,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
  ,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ]
