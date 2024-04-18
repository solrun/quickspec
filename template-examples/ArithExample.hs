{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts #-}
import QuickSpec
arithSig :: [Sig]
arithSig = [
  con "0" (0 :: Int),
  con "1" (1 :: Int),
  con "+" ((+) :: Int -> Int -> Int),
  con "*" ((*) :: Int -> Int -> Int)
  ,template "fix-point"      "?F(?X) = ?X"
  ,template "left-id-elem"   "?F(?Y,X) = X"
  ,template "right-id-elem"  "?F(X,?Y) = X"
  ,template "commutative"    "?F(X,Y) = ?F(Y,X)"
  ,template "op-commute"     "?F(?G(X)) = ?G(?F(X))"
  ,template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))"
  ,template "associative-3"  "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ]

main :: IO ()
main = do
  roughSpec arithSig
