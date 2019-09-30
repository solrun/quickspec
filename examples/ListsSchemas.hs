{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec

main = quickSpec [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  schema "distributive" "?F(?G(X)) = ?G(?F(X))",
  schema "2-distributive" "?F(?G(X),?G(Y)) = ?G(?F(X,Y))",
  schema "commutative" "?F(?G(X,Y)) = ?F(?G(Y,X))",
  schema "id" "?F(?X) = ?X"
  ]
