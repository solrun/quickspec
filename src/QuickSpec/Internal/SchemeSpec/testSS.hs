import QuickSpec.Internal
import QuickSpec.Internal.Type hiding (defaultTo)
import QuickSpec.Internal.Term
import QuickSpec.Internal.SchemeSpec
import qualified QuickSpec.Internal.Haskell as Haskell
import QuickSpec.Internal.SchemeSpec.PropGen
import QuickSpec

--monomap :: (a -> a) -> [a] -> [a]
--monomap = map

mySig = [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  arith (Proxy :: Proxy Int)
  ,template "2-2-distributive" "?F(F,?G(X,Y)) = ?G(?F(F,X),?F(F,Y))"
  ,template "distributive" "?F(?G(X)) = ?G(?F(X))"--
  ,template "2-distributive" "?F(?G(X),?G(Y)) = ?G(?F(X,Y))"
  ,template "commutative" "?F(?G(X,Y)) = ?F(?G(Y,X))"
  ,template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"
  ,template "cancel" "?F(?G(X)) = ?F(X)"
  ,template "cancel-2" "?F(?G(F,X)) = ?F(X)"
  ,template "cancel-3-2" "?F(?G(?H(X))) = ?F(?G(X))"
  ,template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))"
  ,template "comp-id" "?F(?G(X))=X"
  ,template "1-2-distributive" "?F(F,?G(X)) = ?G(?F(F,X))"
  ]

tc = makeConfig mySig
scs = Haskell.cfg_schemas tc
cs = concat $ Haskell.cfg_constants tc
d22 = snd $ head $ scs
d22p = schemaSides d22

main = print $ prettyShow $ head scs
  --schemeSpec tc
